import * as fz from "fuzzaldrin-plus";
import { TsunamiContext } from "../Context";
import { Definition } from "../Indexer";
import { SymbolSearchIndex } from "./SymbolSearchIndex";

export class FuzzAldrinPlusSymbolSearchIndex implements SymbolSearchIndex {
    constructor(private context: TsunamiContext) { }

    public getMatchingSymbols(search?: string): Promise<Definition[]> {
        let results = [...this.context.getIndexedDefinitions()];

        if (search != null) {
            results = fz.filter(results, search, { key: "text" }).slice(0, 10);
        }

        return Promise.resolve(results);
    }
}
