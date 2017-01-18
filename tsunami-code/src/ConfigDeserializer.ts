import { TsunamiCodeConfig } from "./TsunamiCodeConfig";

export class ConfigDeserializer {
    static DEFAULT_CONFIG: TsunamiCodeConfig = {
        importConfig: {
            namespaceAliases: new Map<string, any>()
        },
        formatImportsOnSave: false,
    };

    deserializeConfig(rawConfig: any | undefined): TsunamiCodeConfig {
        if (!rawConfig) {
            return ConfigDeserializer.DEFAULT_CONFIG;
        }

        const namespaceImports = rawConfig.get("namespaceImports", {});
        const importConfig = { namespaceAliases: new Map<any, string>() }; // FIXME bogus types
        Object.keys(namespaceImports).forEach(
            k => importConfig.namespaceAliases.set(k, namespaceImports[k])
        );

        return {
            importConfig,
            formatImportsOnSave: rawConfig.get("formatImportsOnSave", ConfigDeserializer.DEFAULT_CONFIG.formatImportsOnSave)
        };
    }
}
