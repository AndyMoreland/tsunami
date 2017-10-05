import { addImportToFile } from "../importUtilities";
import * as Bluebird from "bluebird";
import * as ts from "typescript";
import * as vs from "vscode";
import { Definition, ImportBlockBuilder, ImportEditor, SimpleImportBlockFormatter, TsunamiContext } from "@derander/tsunami";
import { TsunamiPlugin } from "../TsunamiPlugin";
import { TS_MODE } from "../TypescriptDocumentFilter";
import { toPrettyModuleSpecifier, toTextEdit } from "../util";

class ExternalSymbolCompletionItem extends vs.CompletionItem {
    constructor(
        private context: TsunamiContext,
        private document: vs.TextDocument,
        private definition: Definition
    ) {
        super(definition.text || "<NO_NAME>", vs.CompletionItemKind.Reference);
    }

    get additionalTextEdits(): vs.TextEdit[] {
        const sourceFile = ts.createSourceFile(this.document.fileName, this.document.getText(), ts.ScriptTarget.ES5, true);
        const moduleSpecifier = this.definition.moduleSpecifier.replace(/\.tsx?/g, "") as any;

        const edits = addImportToFile(
            sourceFile,
            this.context.getImportConfig().namespaceAliases,
            moduleSpecifier,
            this.definition.text!,
            this.context.getFormatOptions()
        );

        return edits.map(toTextEdit);
    }

    get detail(): string {
        return toPrettyModuleSpecifier(this.document.fileName, this.definition.moduleSpecifier);
    }

    get insertText(): string {
        const aliases = this.context.getImportConfig().namespaceAliases;
        const moduleSpecifier = this.definition.moduleSpecifier.replace(/\.tsx?/g, "") as any;
        let result = this.definition.text!;

        if (aliases.has(moduleSpecifier)) {
            result = aliases.get(moduleSpecifier) + "." + result;
        }

        return result;
    }
}

export class TsunamiCodeCompletionProvider implements vs.CompletionItemProvider, TsunamiPlugin {
    constructor(private context: TsunamiContext) {}

    public bindToContext(context: vs.ExtensionContext): void {
        context.subscriptions.push(vs.languages.registerCompletionItemProvider(
            TS_MODE,
            this
        ));
    }

    public async provideCompletionItems(
        document: vs.TextDocument,
        position: vs.Position,
        token: vs.CancellationToken
    ): Promise<vs.CompletionItem[]> {
        const matches = await this.context.getMatchingSymbols(
            document.getText(document.getWordRangeAtPosition(position))
        );
        return matches.map(x => {
            return new ExternalSymbolCompletionItem(this.context, document, x);
        });
    }
}
