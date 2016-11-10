import * as vs from "vscode";
import * as tsu from "@derander/tsunami";
import { TsunamiPlugin } from "./TsunamiPlugin";
import { VscodeCommand, VscodeTextEditorCommand } from "./commands/VscodeCommand";

const TS_MODE: vs.DocumentSelector = [
    {
        language: "typescriptreact"
    },
    {
        language: "typescript"
    }
];

export class TsunamiExtension implements TsunamiPlugin {
    constructor(
        private context: tsu.TsunamiContext,
        private plugins: TsunamiPlugin[],
        private commands: VscodeCommand[],
        private textEditorCommands: VscodeTextEditorCommand[]
    ) {}

    private registerCommands(): vs.Disposable[] {
        const disposables: vs.Disposable[] = [];

        for (let command of this.commands) {
            disposables.push(vs.commands.registerCommand(command.commandName, async (...args: any[]) => {
                try {
                    await command.execute(...args);
                } catch (e) {
                    console.error(e);
                }
            }));
        }

        for (let command of this.textEditorCommands) {
            disposables.push(vs.commands.registerTextEditorCommand(
                command.commandName,
                async (editor: vs.TextEditor, edit: vs.TextEditorEdit, ...args: any[]) => {
                    try {
                        await command.execute(editor, edit, ...args);
                    } catch (e) {
                        console.error(e);
                    }
                }
            ));
        }

        return disposables;
    }

    public bindToContext(context: vs.ExtensionContext) {
        const disposables: vs.Disposable[] = this.registerCommands();

        disposables.push(vs.workspace.onDidSaveTextDocument(document => {
            if (vs.languages.match(TS_MODE, document) > 0) {
                this.context.reloadFile(document.fileName, document.getText());
            }
        }));

        this.plugins.forEach(plugin => plugin.bindToContext(context));

        disposables.forEach(d => context.subscriptions.push(d));
    }
}
