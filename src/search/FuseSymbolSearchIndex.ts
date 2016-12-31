import { Definition } from "../Indexer";
import { TsunamiContext } from "../Context";
import { SymbolSearchIndex } from "./SymbolSearchIndex";
import * as fuse from "fuse.js";

export class FuseSymbolSearchIndex implements SymbolSearchIndex {
    constructor(private context: TsunamiContext) {}

    public getMatchingSymbols(query: string): Promise<Definition[]> {
        let results: Definition[] = [];
        this.context.fileIndexerMap.forEach(indexer => {
            results = [...results, ...indexer.getDefinitionIndex().values()];
        });

        this.context.moduleIndexerMap.forEach(indexer => {
            results = [...results, ...indexer.getDefinitionIndex().values()];
        });

        const index = new (fuse as any)(results, {
            keys: ["text"],
            threshold: 0.8
        });

        return Promise.resolve(index.search(query));
    }
}
