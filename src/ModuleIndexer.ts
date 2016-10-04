import { DefinitionIndex, DefinitionType, Indexer } from "./Indexer";

export class ModuleIndexer implements Indexer {
    constructor(private moduleName: string) {}

    public getDefinitionIndex(): DefinitionIndex {
        return {
            [this.moduleName]: {
                default: false,
                filename: this.moduleName,
                text: this.moduleName,
                type: DefinitionType.EXTERNAL_MODULE
            }
        };
    }
}
