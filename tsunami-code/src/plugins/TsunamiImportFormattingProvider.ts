import { TS_MODE } from "../TypescriptDocumentFilter";
import * as ts from "typescript";
import * as vs from "vscode";
import { ImportBlockBuilder, ImportEditor, SimpleImportBlockFormatter, TsunamiContext } from "@derander/tsunami";
import { TsunamiPlugin } from "../TsunamiPlugin";
import { toTextEdit } from "../util";

export class TsunamiImportFormattingProvider implements vs.DocumentFormattingEditProvider, TsunamiPlugin {
    bindToContext(context: vs.ExtensionContext): void {
        vs.languages.registerDocumentFormattingEditProvider(TS_MODE, this);
    }

		  async provideDocumentFormattingEdits(document: vs.TextDocument, options: vs.FormattingOptions): Promise<vs.TextEdit[]> {
        const sourceFile = ts.createSourceFile(document.fileName, document.getText(), ts.ScriptTarget.ES5, true);
        const newBlock = ImportBlockBuilder.fromFile(sourceFile).build();

        const editor = new ImportEditor(new SimpleImportBlockFormatter({
            indentSize: options.tabSize
        }));

        return editor.applyImportBlockToFile(sourceFile, newBlock).map(toTextEdit);
    }
}
