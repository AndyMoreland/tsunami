import { Response } from "./Response";
import { TsProject } from "./tsProject";
import { FileIndexer } from "./FileIndexer";
import { Map } from "./types";
import { TsunamiContext } from "./Context";
import * as ts from "typescript";
import * as fs from "fs";
import * as Promise from "bluebird";

const readFilePromise = Promise.promisify(fs.readFile);

export class MutableTsunamiContext implements TsunamiContext {
    public fileIndexerMap: Map<FileIndexer> = {};
    public moduleIndexerMap: Map<FileIndexer> = {};
    private fileVersionMap: Map<number> = {};

    constructor(
        private project: TsProject,
        public writeOutput: <T>(response: Response<T>) => Promise<void>,
        private documentRegistry: ts.DocumentRegistry
    ) {}

    public getSourceFileFor(filename: string, sourceFileName?: string): Promise<ts.SourceFile> {
        return readFilePromise(sourceFileName || filename).then(file => {
            let sourceText = file.toString();
            this.fileVersionMap[filename] = 1;
            let sourceFile = this.documentRegistry.acquireDocument(filename,
                                                              this.project.getCompilerOptions(),
                                                              ts.ScriptSnapshot.fromString(sourceText), "" + this.fileVersionMap[filename]);
            return sourceFile;
        }).then(foo => this.updateSourceFileFor(filename, sourceFileName));
    }

    public reloadFile(filename: string, tmpfilename: string): Promise<void> {
        return this.updateSourceFileFor(filename, tmpfilename).then(sourceFile => {
            let indexer = new FileIndexer(sourceFile, (filename: string) => this.getSourceFileFor(filename));
            this.fileIndexerMap[filename] = indexer;
            return indexer.indexFile();
        }).thenReturn(null);
    }

    public updateSourceFileFor(filename: string, tmpfilename?: string): Promise<ts.SourceFile> {
        if (this.fileVersionMap[filename] == null || this.fileVersionMap[filename] === undefined) {
            return this.getSourceFileFor(filename, tmpfilename);
        }

        return readFilePromise(tmpfilename || filename).then(file => {
            let sourceText = file.toString();
            this.fileVersionMap[filename] = this.fileVersionMap[filename] + 1;
            let sourceFile = this.documentRegistry.updateDocument(filename,
                                                             this.project.getCompilerOptions(),
                                                             ts.ScriptSnapshot.fromString(sourceText),
                                                             "" + this.fileVersionMap[filename]);
            return sourceFile;
        });
    }
}
