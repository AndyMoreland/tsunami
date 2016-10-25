import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import * as vscode from "vscode";
import * as tsu from "@derander/tsunami";
import { TsunamiCodeActionProvider } from "./TsunamiCodeActionProvider";
import { TsunamiCodeCompletionProvider } from "./TsunamiCodeCompletionProvider";
import { toPrettyModuleSpecifier } from "./util";

const TS_MODE: vscode.DocumentFilter = {
    language: "typescript",
    scheme: "file"
};

function applyCodeEdit(editBuilder: vscode.TextEditorEdit, edit: tsu.CodeEdit): void {
    editBuilder.replace(new vscode.Range(
        edit.start.line - 1,
        edit.start.offset - 1,
        edit.end.line - 1,
        edit.end.offset - 1
    ), edit.newText);
}

interface CompletionItem {
    label: string;
    description: string;
    definition: tsu.Definition;
}

export function activate(context: vscode.ExtensionContext) {
    const projectRoot = vscode.workspace.rootPath;
    console.log("Activating!");

    /* Tsunami is only available in projects. */
    if (!projectRoot) {
        return;
    }

    if (!fs.existsSync(path.join(projectRoot, "tsconfig.json"))) {
        console.log("Aborting tsunami initialization: couldn't find tsconfig at ", path.join(projectRoot, "tsconfig.json"));
        return;
    }

    const settings = JSON.parse(fs.readFileSync(path.join(projectRoot, "tsconfig.json")).toString());

    const tsunami = new tsu.Tsunami(
        new tsu.TsProject(projectRoot, settings)
    );

    tsunami.buildInitialProjectIndex()
        .then(() => vscode.window.setStatusBarMessage("[tsunami] $(thumbsup) Done indexing: " + path.basename(projectRoot), 3000))
        .catch(e => console.error(e));

    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json

    console.log("Registering commands!");

    const firstCommand = vscode.commands.registerCommand("tsunami.reindexProject", () => {
        tsunami.buildInitialProjectIndex().catch(e => console.error(e));
    });

    const secondCommand = vscode.commands.registerTextEditorCommand("tsunami.importSymbol", importSymbolCommand);

    async function importSymbolCommand(editor: vscode.TextEditor, edit: vscode.TextEditorEdit, symbol?: string) {
        // The code you place here will be executed every time your command is executed
        const context = tsunami.getContext();
        let results: CompletionItem[] = [];
        const exactMatchResults: CompletionItem[] = [];

        const definitions = await context.getMatchingSymbols(symbol);
        definitions.forEach(def => {
            const item = {
                definition: def,
                label: def.text || "",
                description: toPrettyModuleSpecifier(editor.document.fileName, def.moduleSpecifier)
            };
            results.push(item);
            if (symbol && def.text === symbol) {
                exactMatchResults.push(item);
            }
        });

        if (results.length === 0) {
            vscode.window.showErrorMessage("No matching symbols found in project.");
            return;
        }

        if (exactMatchResults.length > 0) {
            results = exactMatchResults;
        }

        const choice = results.length === 1 ? results[0] : await vscode.window.showQuickPick(results);
        if (!choice) {
            return;
        }
        const sourceFile = ts.createSourceFile(editor.document.fileName, editor.document.getText(), ts.ScriptTarget.ES5, true);
        const newBlock = tsu.ImportBlockBuilder.fromFile(sourceFile)
            .addImportBinding(choice.definition.moduleSpecifier.replace(/\.tsx?/g, "") as any,
                              { symbolName: choice.definition.text || "" })
            .build();
        const edits = (new tsu.ImportEditor(new tsu.SimpleImportBlockFormatter()))
            .applyImportBlockToFile(sourceFile, newBlock);
        editor.edit((editBuilder) => applyCodeEdit(editBuilder, edits[0]));
        editor.edit((editBuilder) => {
            const currentWordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
            if (currentWordRange) {
                return editBuilder.replace(currentWordRange, choice.label);
            }
        });
    }

    vscode.languages.registerCompletionItemProvider(TS_MODE, new TsunamiCodeCompletionProvider(tsunami.getContext()));
    context.subscriptions.push(firstCommand, secondCommand);
    context.subscriptions.push(vscode.languages.registerCodeActionsProvider(TS_MODE, new TsunamiCodeActionProvider()));
    context.subscriptions.push(vscode.languages.registerCompletionItemProvider(
        TS_MODE,
        new TsunamiCodeCompletionProvider(tsunami.getContext())
    ));
    context.subscriptions.push(vscode.workspace.onDidSaveTextDocument(document => {
        if (vscode.languages.match(TS_MODE, document) > 0) {
            tsunami.getContext().reloadFile(document.fileName, document.getText());
        }
    }));
}

// this method is called when your extension is deactivated
export function deactivate() {
    /* do nothing */
}
