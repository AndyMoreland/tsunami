import * as vscode from "vscode";
import { TsunamiPlugin } from "../TsunamiPlugin";
import { TS_MODE } from "../TypescriptDocumentFilter";

export class TsunamiCodeActionProvider implements vscode.CodeActionProvider, TsunamiPlugin {
    public bindToContext(context: vscode.ExtensionContext): void {
        context.subscriptions.push(vscode.languages.registerCodeActionsProvider(
            TS_MODE,
            this
        ));
    }

    public async provideCodeActions(
        document: vscode.TextDocument,
        range: vscode.Range,
        context: vscode.CodeActionContext,
        token: vscode.CancellationToken
    ): Promise<vscode.Command[]> {
        if (context.diagnostics.find(x => x.message.indexOf("Cannot find name") >= 0)) {
            const wordRange = document.getWordRangeAtPosition(range.start);
            const symbol = document.getText(wordRange);
            return [
                {
                    title: "(tsu) Import " + symbol,
                    command: "tsunami.importSymbol",
                    arguments: [symbol]
                }
            ];
        } else {
            return [];
        }
    }
}
