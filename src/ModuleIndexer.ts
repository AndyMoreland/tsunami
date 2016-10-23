import { DefinitionIndex, DefinitionType, Indexer } from "./Indexer";
import { ModuleName } from "./imports/ImportStatement";

export class ModuleIndexer implements Indexer {
    constructor(private moduleName: string) {}

    public getDefinitionIndex(): DefinitionIndex {
        return new Map([[
            this.moduleName,
            {
                default: false,
                moduleSpecifier: this.moduleName as ModuleName,
                span: undefined!,
                text: this.moduleName,
                type: DefinitionType.EXTERNAL_MODULE,
            }
        ]]);
    }
}
