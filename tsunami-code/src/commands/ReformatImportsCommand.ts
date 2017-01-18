import { applyCodeEdit } from "../util";
import * as ts from "typescript";
import * as vs from "vscode";
import * as tsu from "@derander/tsunami";
import { VscodeTextEditorCommand } from "./VscodeCommand";

export class ReformatImportsCommand implements VscodeTextEditorCommand {
    commandName = "tsunami.reformatImports";

    constructor(
        private context: tsu.TsunamiContext
    ) {}

    async execute(editor: vs.TextEditor, edit: vs.TextEditorEdit, ...args: any[]): Promise<void> {
        const sourceFile = ts.createSourceFile(editor.document.fileName, editor.document.getText(), ts.ScriptTarget.ES5, true);
        const newBlock = tsu.ImportBlockBuilder.fromFile(sourceFile).build();
        const edits = new tsu.ImportEditor(new tsu.SimpleImportBlockFormatter(
            this.context.getFormatOptions()
        )).applyImportBlockToFile(sourceFile, newBlock);

        await Promise.resolve(editor.edit(builder => edits.forEach(edit => applyCodeEdit(builder, edit))));
    }
}
