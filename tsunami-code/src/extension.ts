import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import * as tsu from "@derander/tsunami";
import { TsunamiExtension } from "./TsunamiExtension";
import { ImportSymbolCommand } from "./commands/ImportSymbolCommand";
import { ReindexProjectCommand } from "./commands/ReindexProjectCommand";
import { TsunamiCodeActionProvider } from "./plugins/TsunamiCodeActionProvider";
import { TsunamiCodeCompletionProvider } from "./plugins/TsunamiCodeCompletionProvider";

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

    const config = vscode.workspace.getConfiguration("tsunami").get("namespaceImports", {});
    const importConfig =  { namespaceAliases: new Map<any, string>() }; // FIXME bogus types
    Object.keys(config).forEach(k => importConfig.namespaceAliases.set(k, config[k]));
    const project = await tsu.TsProject.fromRootDir(projectRoot);
    const tsunami = new tsu.Tsunami(
        project,
        tsu.buildFormatOptions(),
        importConfig
    );

    const extension = new TsunamiExtension(
        tsunami.getContext(),
        [
            new TsunamiCodeCompletionProvider(tsunami.getContext()),
            new TsunamiCodeActionProvider()
        ],
        [
            new ReindexProjectCommand(tsunami)
        ],
        [
            new ImportSymbolCommand(tsunami.getContext())
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
