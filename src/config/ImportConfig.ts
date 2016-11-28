import { ModuleSpecifier } from "../imports/ImportStatement";

export interface ImportConfig {
    namespaceAliases: Map<ModuleSpecifier, string>;
}
