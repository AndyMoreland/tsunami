import { ModuleIndexer } from "./ModuleIndexer";
import { FileIndexer } from "./FileIndexer";
import { Map } from "./types";
import { Response } from "./Response";
import * as ts from "typescript";
import * as Promise from "bluebird";

export interface TsunamiContext {
    getSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    updateSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    reloadFile(filename: string, tmpfilename?: string): Promise<void>;
    writeOutput<T>(response: Response<T>): Promise<void>;
    fileIndexerMap: Map<FileIndexer>;
    moduleIndexerMap: Map<FileIndexer>;
}
