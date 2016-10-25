import * as vscode from "vscode";

export class TsunamiCodeActionProvider implements vscode.CodeActionProvider {
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
                    title: "Import " + symbol,
                    command: "tsunami.importSymbol",
                    arguments: [symbol]
                }
            ];
        } else {
            return [];
        }
    }
}
