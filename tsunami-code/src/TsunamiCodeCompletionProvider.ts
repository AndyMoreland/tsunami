import * as Bluebird from "bluebird";
import { TsunamiContext } from "@derander/tsunami";
import * as vsc from "vscode";


export class TsunamiCodeCompletionProvider implements vsc.CompletionItemProvider {
    constructor(private context: TsunamiContext) {}
    

    async provideCompletionItems(document: vsc.TextDocument, position: vsc.Position, token: vsc.CancellationToken): Bluebird<vsc.CompletionItem[]> {
        const matches = await this.context.getMatchingSymbols("");

        return matches.map(x => {
            return new vsc.CompletionItem(x.text || "")
        });
    }
}
