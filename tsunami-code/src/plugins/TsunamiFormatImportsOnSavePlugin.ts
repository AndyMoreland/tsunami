import * as vs from "vscode";
import { TsunamiPlugin } from "../TsunamiPlugin";
import { TS_MODE } from "../TypescriptDocumentFilter";
import { applyTextEdit, toTextEdit } from "../util";
import { TsunamiImportFormattingProvider } from "./TsunamiImportFormattingProvider";

export class TsunamiFormatImportsOnSavePlugin implements TsunamiPlugin {
    constructor(
        private formattingProvider: TsunamiImportFormattingProvider
    ) {}

    bindToContext(context: vs.ExtensionContext): void {
        /* concept cribbed from vscode-go */
        const alreadyAppliedFormatting = new WeakSet<vs.TextDocument>();

        context.subscriptions.push(
            vs.workspace.onDidSaveTextDocument(async document => {
                if (vs.languages.match(TS_MODE, document)) {
                    const editor = vs.window.activeTextEditor;
                    if (editor.document === document && !alreadyAppliedFormatting.has(document)) {
                        const edits = await this.formattingProvider.provideDocumentFormattingEdits(
                            document, {
                                tabSize: editor.options.tabSize as number,
                                insertSpaces: editor.options.insertSpaces as boolean
                            }
                        );

                        await Promise.all(edits.map(edit => editor.edit(builder => applyTextEdit(builder, edit))));

                        alreadyAppliedFormatting.add(document);

                        return document.save;
                    }
                } else {
                    alreadyAppliedFormatting.delete(document);
                }
            })
        );
    }
}
