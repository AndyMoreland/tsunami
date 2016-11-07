import * as Promise from "bluebird";
import * as ts from "typescript";
import { FileIndexer } from "./FileIndexer";
import { Definition } from "./Indexer";
import { Response } from "./Response";
import { ImportBlockFormatterOptions } from "./imports/ImportBlockFormatter";
import { TsProject } from "./tsProject";

export interface TsunamiContext {
    getSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    updateSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    reloadFile(filename: string, fileText?: string): Promise<ts.SourceFile>;
    writeOutput<T>(response: Response<T>): Promise<void>;
    getProject(): TsProject;
    fileIndexerMap: Map<string, FileIndexer>;
    moduleIndexerMap: Map<string, FileIndexer>;
    getMatchingSymbols(search?: string): Promise<Definition[]>;
    getProgram(): Promise<ts.Program>;
    getIndexedDefinitions(): IterableIterator<Definition>;
    getFormatOptions(): ImportBlockFormatterOptions;
}
