
import * as Bluebird from "bluebird";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { TsunamiContext } from "./Context";
import { FileIndexer } from "./FileIndexer";
import { Definition } from "./Indexer";
import { Response } from "./Response";
import { InitializedFormatOptions } from "./formatting/FormatOptions";
import { AbsoluteFilename } from "./imports/ImportStatement";
import log from "./log";
import { FuzzAldrinPlusSymbolSearchIndex } from "./search/FuzzAldrinPlusSymbolSearchIndex";
import { SymbolSearchIndex } from "./search/SymbolSearchIndex";
import { TsProject } from "./tsProject";

const readFilePromise = Bluebird.promisify(fs.readFile);

export class MutableTsunamiContext implements TsunamiContext {
    public fileIndexerMap: Map<string, FileIndexer> = new Map<string, FileIndexer>();
    public moduleIndexerMap: Map<string, FileIndexer> = new Map<string, FileIndexer>();
    private fileVersionMap: Map<string, number> = new Map<string, number>();
    private symbolSearchIndex: SymbolSearchIndex;

    constructor(
        private project: TsProject,
        private formatOptions: InitializedFormatOptions,
        public writeOutput: <T>(response: Response<T>) => Promise<void>,
        private documentRegistry: ts.DocumentRegistry
    ) {
        this.symbolSearchIndex = new FuzzAldrinPlusSymbolSearchIndex(this);
    }

    public getSourceFileForSync(filename: string, sourceFileName?: string): ts.SourceFile {
        const file = fs.readFileSync(sourceFileName || filename);
        let sourceText = file.toString();
        this.fileVersionMap.set(filename, 1);
        let sourceFile = this.documentRegistry.acquireDocument(
            filename,
            this.project.getCompilerOptions(),
            ts.ScriptSnapshot.fromString(sourceText), "" + this.fileVersionMap.get(filename));
        this.updateSourceFileFor(filename, sourceFileName);
        return sourceFile;
    }

    public async getSourceFileFor(filename: string, fileText?: string): Promise<ts.SourceFile> {
        const sourceText = fileText == null ? (await readFilePromise(filename)).toString() : fileText;
        this.fileVersionMap.set(filename, (this.fileVersionMap.get(filename) || 0) + 1);
        this.documentRegistry.acquireDocument(
            filename,
            this.project.getCompilerOptions(),
            ts.ScriptSnapshot.fromString(sourceText), "" + this.fileVersionMap.get(filename));

        return this.updateSourceFileFor(filename, fileText);
    }

    public async reloadFile(filename: string, fileText?: string): Promise<ts.SourceFile> {
        log("reloading ", filename);
        const sourceFile = await this.updateSourceFileFor(filename, fileText);
        let indexer = new FileIndexer(
            sourceFile.fileName as AbsoluteFilename,
            sourceFile,
            filename => this.getSourceFileFor(filename)
        );

        this.fileIndexerMap.set(filename, indexer);
        await indexer.indexFile();

        return sourceFile;
    }

    public async updateSourceFileFor(filename: string, fileText?: string): Promise<ts.SourceFile> {
        if (this.fileVersionMap.get(filename) == null) {
            return this.getSourceFileFor(filename, fileText);
        }

        const sourceText = fileText == null ? (await readFilePromise(filename)).toString() : fileText;
        this.fileVersionMap.set(filename, this.fileVersionMap.get(filename) + 1);
        const sourceFile = this.documentRegistry.updateDocument(filename,
                                                                this.project.getCompilerOptions(),
                                                                ts.ScriptSnapshot.fromString(sourceText),
                                                                "" + this.fileVersionMap.get(filename));
        return sourceFile;
    }

    public getProject(): TsProject {
        return this.project;
    }

    public getMatchingSymbols(search?: string): Promise<Definition[]> {
        return this.symbolSearchIndex.getMatchingSymbols(search);
    }

    public async getProgram(): Promise<ts.Program> {
        const project = this.getProject();
        let files = await project.getAllFilenames();
        files = files.map(f => path.resolve(project.getRoot(), f));
        console.log(project.getCompilerOptions());
        const program = ts.createProgram(
            files,
            project.getCompilerOptions(),
            ts.createCompilerHost(project.getCompilerOptions(), true)
        );

        return program;
    }

    public * getIndexedDefinitions(): IterableIterator<Definition> {
        for (let index of this.fileIndexerMap.values()) {
            yield* index.getDefinitionIndex().values();
        }

        for (let index of this.moduleIndexerMap.values()) {
            yield* index.getDefinitionIndex().values();
        }
    }

    public getFormatOptions(): InitializedFormatOptions {
        return this.formatOptions;
    }
}
