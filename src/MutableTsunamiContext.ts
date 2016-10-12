import * as Promise from "bluebird";
import * as fs from "fs";
import * as ts from "typescript";
import { TsunamiContext } from "./Context";
import { FileIndexer } from "./FileIndexer";
import { Definition } from "./Indexer";
import { Response } from "./Response";
import { AbsoluteFilename } from "./imports/ImportStatement";
import { TsProject } from "./tsProject";

const readFilePromise = Promise.promisify(fs.readFile);

export class MutableTsunamiContext implements TsunamiContext {
    public fileIndexerMap: Map<string, FileIndexer> = new Map<string, FileIndexer>();
    public moduleIndexerMap: Map<string, FileIndexer> = new Map<string, FileIndexer>();
    private fileVersionMap: Map<string, number> = new Map<string, number>();

    constructor(
        private project: TsProject,
        public writeOutput: <T>(response: Response<T>) => Promise<void>,
        private documentRegistry: ts.DocumentRegistry
    ) {}

    public getSourceFileFor(filename: string, sourceFileName?: string): Promise<ts.SourceFile> {
        return readFilePromise(sourceFileName || filename).then(file => {
            let sourceText = file.toString();
            this.fileVersionMap.set(filename, 1);
            let sourceFile = this.documentRegistry.acquireDocument(
                filename,
                this.project.getCompilerOptions(),
                ts.ScriptSnapshot.fromString(sourceText), "" + this.fileVersionMap.get(filename));
            return sourceFile;
        }).then(foo => this.updateSourceFileFor(filename, sourceFileName));
    }

    public reloadFile(filename: string, tmpfilename: string): Promise<void> {
        return this.updateSourceFileFor(filename, tmpfilename).then(sourceFile => {
            let indexer = new FileIndexer(
                sourceFile.fileName as AbsoluteFilename,
                sourceFile,
                filename => this.getSourceFileFor(filename)
            );
            this.fileIndexerMap.set(filename, indexer);
            return indexer.indexFile();
        }).thenReturn(null!);
    }

    public updateSourceFileFor(filename: string, tmpfilename?: string): Promise<ts.SourceFile> {
        if (this.fileVersionMap.get(filename) == null) {
            return this.getSourceFileFor(filename, tmpfilename);
        }

        return readFilePromise(tmpfilename || filename).then(file => {
            const sourceText = file.toString();
            this.fileVersionMap.set(filename, this.fileVersionMap.get(filename) + 1);
            const sourceFile = this.documentRegistry.updateDocument(filename,
                                                                    this.project.getCompilerOptions(),
                                                                    ts.ScriptSnapshot.fromString(sourceText),
                                                                    "" + this.fileVersionMap.get(filename));
            return sourceFile;
        });
    }

    public getProject(): TsProject {
        return this.project;
    }

    public getMatchingSymbols(search: string): Promise<Definition[]> {
        const results: Definition[] = [];
        this.fileIndexerMap.forEach(indexer => {
            for (let definition of indexer.getDefinitionIndex().values()) {
                results.push(definition);
            }
        });

        this.moduleIndexerMap.forEach(indexer => {
            for (let definition of indexer.getDefinitionIndex().values()) {
                results.push(definition);
            }
        });

        return Promise.resolve(results);
    }
}
