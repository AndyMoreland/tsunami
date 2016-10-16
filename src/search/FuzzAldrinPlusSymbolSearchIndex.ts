import * as fz from "fuzzaldrin-plus";
import { TsunamiContext } from "../Context";
import { Definition } from "../Indexer";
import { SymbolSearchIndex } from "./SymbolSearchIndex";

export class FuzzAldrinPlusSymbolSearchIndex implements SymbolSearchIndex {
    constructor(private context: TsunamiContext) {}

    public getMatchingSymbols(search?: string): Promise<Definition[]> {
        let results: Definition[] = [];
        this.context.fileIndexerMap.forEach(indexer => {
            results = [...results, ...indexer.getDefinitionIndex().values()];
        });

        this.context.moduleIndexerMap.forEach(indexer => {
            results = [...results, ...indexer.getDefinitionIndex().values()];
        });

        if (search != null) {
            results = fz.filter(results, search, { key: "text" });
        }

        return Promise.resolve(results);
    }
}
