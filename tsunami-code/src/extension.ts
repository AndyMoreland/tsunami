import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import * as tsu from "@derander/tsunami";
import { ConfigDeserializer } from "./ConfigDeserializer";
import { TsunamiExtension } from "./TsunamiExtension";
import { TsunamiPlugin } from "./TsunamiPlugin";
import { ImportSymbolCommand } from "./commands/ImportSymbolCommand";
import { ReformatImportsCommand } from "./commands/ReformatImportsCommand";
import { ReindexProjectCommand } from "./commands/ReindexProjectCommand";
import { TsunamiCodeActionProvider } from "./plugins/TsunamiCodeActionProvider";
import { TsunamiCodeCompletionProvider } from "./plugins/TsunamiCodeCompletionProvider";
import { TsunamiFormatImportsOnSavePlugin } from "./plugins/TsunamiFormatImportsOnSavePlugin";
import { TsunamiImportFormattingProvider } from "./plugins/TsunamiImportFormattingProvider";

export async function activate(context: vscode.ExtensionContext): Promise<void> {
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

    const configDeserializer = new ConfigDeserializer();
    const config = configDeserializer.deserializeConfig(
        vscode.workspace.getConfiguration("tsunami"),
        {
            tabSize: vscode.workspace.getConfiguration("editor").get("tabSize", 2)
        }
    );
    console.log("Deserialized config: ", config);
    const project = await tsu.TsProject.fromRootDir(projectRoot);
    const tsunami = new tsu.Tsunami(
        project,
        tsu.buildFormatOptions(config.formatOptions),
        config.importConfig
    );

    const formattingProvider = new TsunamiImportFormattingProvider(tsunami.getContext());

    const plugins: TsunamiPlugin[] = [
        new TsunamiCodeCompletionProvider(tsunami.getContext()),
        new TsunamiCodeActionProvider(),
        formattingProvider,
    ];

    if (config.formatImportsOnSave) {
        plugins.push(new TsunamiFormatImportsOnSavePlugin(formattingProvider));
    }

    const extension = new TsunamiExtension(
        tsunami.getContext(),
        plugins,
        [
            new ReindexProjectCommand(tsunami),
        ],
        [
            new ImportSymbolCommand(tsunami.getContext()),
            new ReformatImportsCommand(tsunami.getContext()),
        ]
    );

    extension.bindToContext(context);

    tsunami.buildInitialProjectIndex()
        .then(() => vscode.window.setStatusBarMessage("[tsunami] $(thumbsup) Done indexing: " + path.basename(projectRoot), 3000))
        .catch(e => console.error(e));
}

export function deactivate() {
    /* do nothing */
}
