import * as vscode from "vscode";

export class TsunamiCodeActionProvider implements vscode.CodeActionProvider {
    public provideCodeActions(
        document: vscode.TextDocument,
        range: vscode.Range,
        context: vscode.CodeActionContext,
        token: vscode.CancellationToken
    ): Thenable<vscode.Command[]> {
        if (context.diagnostics.find(x => x.message.indexOf("Cannot find name") >= 0)) {
            const symbol = document.getText(range);
            return Promise.resolve([
                {
                    title: "Import " + symbol,
                    command: "tsunami.importSymbol",
                    arguments: [symbol]
                }
            ]);
        } else {
            return Promise.resolve([]);
        }
    }
}
