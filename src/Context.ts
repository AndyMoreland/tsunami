import * as Promise from "bluebird";
import * as ts from "typescript";
import { FileIndexer } from "./FileIndexer";
import { ModuleIndexer } from "./ModuleIndexer";
import { Response } from "./Response";
import { TsProject } from "./tsProject";
import { Map } from "./types";

export interface TsunamiContext {
    getSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    updateSourceFileFor(fileName: string, tmpFileName?: string): Promise<ts.SourceFile>;
    reloadFile(filename: string, tmpfilename?: string): Promise<void>;
    writeOutput<T>(response: Response<T>): Promise<void>;
    getProject(): TsProject;
    fileIndexerMap: Map<FileIndexer>;
    moduleIndexerMap: Map<FileIndexer>;
}
