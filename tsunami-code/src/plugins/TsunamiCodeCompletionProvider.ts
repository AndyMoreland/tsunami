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
        const newBlock = ImportBlockBuilder.fromFile(sourceFile)
            .addImportBinding(this.definition.moduleSpecifier.replace(/\.tsx?/g, "") as any,
                              { symbolName: this.definition.text || "" })
            .build();
        const edits = (new ImportEditor(new SimpleImportBlockFormatter()))
            .applyImportBlockToFile(sourceFile, newBlock);

        return edits.map(toTextEdit);
    }

    get detail(): string {
        return toPrettyModuleSpecifier(this.document.fileName, this.definition.moduleSpecifier);
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
    ): Bluebird<vs.CompletionItem[]> {
        const matches = await this.context.getMatchingSymbols(
            document.getText(document.getWordRangeAtPosition(position))
        );
        return matches.map(x => {
            return new ExternalSymbolCompletionItem(this.context, document, x);
        });
    }
}
