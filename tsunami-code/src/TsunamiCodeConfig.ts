
import { InitializedFormatOptions } from "@derander/tsunami";

export interface TsunamiCodeConfig {
    importConfig: {
        namespaceAliases: Map<any, string>;
    };
    formatImportsOnSave: boolean;
    formatOptions: InitializedFormatOptions;
    enableCompletionProvider: boolean;
    enableImportCodeAction: boolean;
}
