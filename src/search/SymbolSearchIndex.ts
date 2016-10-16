import { Definition } from "../Indexer";

export interface SymbolSearchIndex {
    getMatchingSymbols(search?: string): Promise<Definition[]>;
}
