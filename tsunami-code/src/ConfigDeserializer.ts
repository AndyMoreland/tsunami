import * as vs from "vscode";
import { TsunamiCodeConfig } from "./TsunamiCodeConfig";

export class ConfigDeserializer {
    getDefaultConfig(vscodeOptions: vs.TextEditorOptions): TsunamiCodeConfig {
        return {
            importConfig: {
                namespaceAliases: new Map<string, any>(),
            },
            formatOptions: {
                indentSize: vscodeOptions.tabSize as number,
                trailingCommaInObjectLiterals: false,
                useDoubleQuotes: true,
            },
            formatImportsOnSave: false,
            enableCompletionProvider: true,
        };
    };

    deserializeConfig(rawConfig: any | undefined, vscodeOptions: vs.TextEditorOptions): TsunamiCodeConfig {
        const defaultConfig =  this.getDefaultConfig(vscodeOptions);
        if (!rawConfig) {
            return defaultConfig;
        }

        const namespaceImports = rawConfig.get("namespaceImports", {});
        const importConfig = {
            namespaceAliases: new Map<any, string>(),
        }; // FIXME bogus types
        Object.keys(namespaceImports).forEach(
            k => importConfig.namespaceAliases.set(k, namespaceImports[k])
        );

        return {
            importConfig,
            formatOptions: {
                indentSize: defaultConfig.formatOptions.indentSize,
                trailingCommaInObjectLiterals: rawConfig.get(
                    "trailingCommaInObjectLiterals",
                    defaultConfig.formatOptions.trailingCommaInObjectLiterals
                ),
                useDoubleQuotes: rawConfig.get(
                    "formatUseDoubleQuotes",
                    defaultConfig.formatOptions.useDoubleQuotes
                ),
            },
            formatImportsOnSave: rawConfig.get("formatImportsOnSave", defaultConfig.formatImportsOnSave),
            enableCompletionProvider: rawConfig.get("enableCompletionProvider", defaultConfig.enableCompletionProvider),
        };
    }
}
