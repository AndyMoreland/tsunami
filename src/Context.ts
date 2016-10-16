import * as Promise from "bluebird";
import * as ts from "typescript";
import { FileIndexer } from "./FileIndexer";
import { Definition } from "./Indexer";
import { Response } from "./Response";
import { TsProject } from "./tsProject";

export interface TsunamiContext {
    getSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    updateSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    reloadFile(filename: string, tmpfilename?: string): Promise<void>;
    writeOutput<T>(response: Response<T>): Promise<void>;
    getProject(): TsProject;
    fileIndexerMap: Map<string, FileIndexer>;
    moduleIndexerMap: Map<string, FileIndexer>;
    getMatchingSymbols(search?: string): Promise<Definition[]>;
}
