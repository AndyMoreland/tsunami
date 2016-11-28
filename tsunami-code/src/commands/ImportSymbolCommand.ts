import * as ts from "typescript";
import * as vs from "vscode";
import * as tsu from "@derander/tsunami";
import { applyCodeEdit, toPrettyModuleSpecifier } from "../util";
import { VscodeTextEditorCommand } from "./VscodeCommand";

interface CompletionItem {
    label: string;
    description: string;
    definition: tsu.Definition;
}

export class ImportSymbolCommand implements VscodeTextEditorCommand {
    public commandName = "tsunami.importSymbol";

    constructor(private context: tsu.TsunamiContext) { }

    private async getChoice(rootPath: string, symbol?: string): Promise<CompletionItem | undefined> {
        let results: CompletionItem[] = [];
        const exactMatchResults: CompletionItem[] = [];
        const definitions = await this.context.getMatchingSymbols(symbol);
        definitions.forEach(def => {
            const item = {
                definition: def,
                label: def.text || "",
                description: toPrettyModuleSpecifier(rootPath, def.moduleSpecifier)
            };
            results.push(item);
            if (symbol && def.text === symbol) {
                exactMatchResults.push(item);
            }
        });

        if (results.length === 0) {
            vs.window.showErrorMessage("No matching symbols found in project.");
            return undefined;
        }

        if (exactMatchResults.length > 0) {
            results = exactMatchResults;
        }

        return (results.length === 1) ? results[0] : await vs.window.showQuickPick(results);
    }

    private async editImportBlock(editor: vs.TextEditor, definition: tsu.Definition): Promise<void> {
        const sourceFile = ts.createSourceFile(editor.document.fileName, editor.document.getText(), ts.ScriptTarget.ES5, true);
        const newBlock = tsu.ImportBlockBuilder.fromFile(sourceFile)
            .addImportBinding(
                definition.moduleSpecifier.replace(/\.tsx?/g, "") as any,
                { symbolName: definition.text || "" }
            )
            .build();
        const edits = (new tsu.ImportEditor(new tsu.SimpleImportBlockFormatter()))
            .applyImportBlockToFile(sourceFile, newBlock);
        editor.edit((editBuilder) => applyCodeEdit(editBuilder, edits[0]));
    }

    private async editSymbolAtPoint(editor: vs.TextEditor, chosenSymbolName: string): Promise<void> {
        editor.edit((editBuilder) => {
            const currentWordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
            if (currentWordRange) {
                return editBuilder.replace(currentWordRange, chosenSymbolName);
            }
        });
    }

    public async execute(editor: vs.TextEditor, edit: vs.TextEditorEdit, symbol?: string): Promise<void> {
        // The code you place here will be executed every time your command is executed
        symbol = typeof symbol === "object" ? undefined : symbol;

        const choice = await this.getChoice(editor.document.fileName, symbol);
        if (!choice) {
            return;
        }

        await this.editImportBlock(editor, choice.definition);
        await this.editSymbolAtPoint(editor, choice.label);
    }
}
