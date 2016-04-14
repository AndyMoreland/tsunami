import { DefinitionType, Indexer, DefinitionIndex } from "./Indexer";

export class ModuleIndexer implements Indexer {
    constructor(private moduleName: string) {}

    public getDefinitionIndex(): DefinitionIndex {
        return {
            [this.moduleName]: {
                default: false,
                filename: this.moduleName,
                location: null,
                text: this.moduleName,
                type: DefinitionType.EXTERNAL_MODULE
            }
        };
    }
}
