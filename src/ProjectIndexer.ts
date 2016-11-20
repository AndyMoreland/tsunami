import * as Bluebird from "bluebird";
import * as fs from "fs";
import { TsunamiContext } from "./Context";
import { FileIndexer } from "./FileIndexer";
import { ModuleName } from "./imports/ImportStatement";
import log from "./log";
import { TsProject } from "./tsProject";

export class ProjectIndexer {
    constructor(
        private project: TsProject,
        private context: TsunamiContext
    ) {}

    public async indexProject(): Promise<void> {
        await Bluebird.all([
            this.indexProjectFiles(),
            this.indexDependenciesOfProject()
        ]);
    }

    private async indexProjectFiles(): Promise<void> {
        const files = await this.project.getFileNames();

        log(JSON.stringify(files, null, 2));
        const promises = files.map(file => this.context.getSourceFileFor(file).then(() => {
            log("Indexing: ", file);
            return this.context.reloadFile(file);
        }));

        await Bluebird.all(promises);
        log("Finished starting server.");
    }

    private async indexDependenciesOfProject(): Promise<void> {
        try {
            const deps = await this.project.getDependencyFilenames();
            log("Dependency typings: ", JSON.stringify(deps, null, 2));
            const promises: Promise<void>[] = [];

            Object.keys(deps).forEach(dep => {
                try {
                    fs.accessSync(deps[dep]);
                    let typings = deps[dep];

                    if (typings) {
                        promises.push(this.indexDefinitionFile(dep, typings));
                    }
                } catch (e) {
                    log("Failed to index: ", dep);
                }
            });

            await Bluebird.all(promises);
        } catch (error) {
            log("Failed to get dependency filenames.");
            log(error.stack);
        }
    }

    private async indexDefinitionFile(moduleName: string, filename: string): Promise<void> {
        await this.context.getSourceFileFor(filename);
        const sourceFile = await this.context.updateSourceFileFor(filename); // FIXME: Why?????????
        log("Indexing definition file:", sourceFile.fileName, moduleName);
        const indexer = new FileIndexer(
            moduleName as ModuleName,
            sourceFile,
            fileName => this.context.getSourceFileFor(fileName)
        );
        this.context.moduleIndexerMap.set(moduleName, indexer);
        return indexer.indexFile();
    }
}
