import * as Bluebird from "bluebird";
import * as ts from "typescript";
import * as vsc from "vscode";
import { Definition, ImportBlockBuilder, ImportEditor, SimpleImportBlockFormatter, TsunamiContext } from "@derander/tsunami";
import { toPrettyModuleSpecifier, toTextEdit } from "./util";

class ExternalSymbolCompletionItem extends vsc.CompletionItem {
    constructor(
        private context: TsunamiContext,
        private document: vsc.TextDocument,
        private definition: Definition
    ) {
        super(definition.text || "<NO_NAME>", vsc.CompletionItemKind.Reference);
    }

    get additionalTextEdits(): vsc.TextEdit[] {
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

export class TsunamiCodeCompletionProvider implements vsc.CompletionItemProvider {
    constructor(private context: TsunamiContext) {}

    public async provideCompletionItems(
        document: vsc.TextDocument,
        position: vsc.Position,
        token: vsc.CancellationToken
    ): Bluebird<vsc.CompletionItem[]> {
        const matches = await this.context.getMatchingSymbols(
            document.getText(document.getWordRangeAtPosition(position))
        );
        return matches.map(x => {
            return new ExternalSymbolCompletionItem(this.context, document, x);
        });
    }
}
