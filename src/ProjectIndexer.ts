import * as Bluebird from "bluebird";
import * as fs from "fs";
import { TsunamiContext } from "./Context";
import { FileIndexer } from "./FileIndexer";
import { ModuleName } from "./imports/ImportStatement";
import log from "./log";
import { TsProject } from "./tsProject";

/**
 * Strip @types/ from module names
 */
function getModuleName(rawModuleName: string): string {
    if (rawModuleName.startsWith("@types/")) {
        return rawModuleName.substring("@types/".length);
    }

    return rawModuleName;
}

export class ProjectIndexer {
    constructor(private project: TsProject, private context: TsunamiContext) {}

    public async indexProject(): Promise<void> {
        await Bluebird.all([
            this.indexProjectFiles(),
            this.indexDependenciesOfProject(),
            this.indexMappedFiles()
        ]);
    }

    private async indexMappedFiles(): Promise<void> {
        const paths = this.project.getCompilerOptions().paths;
        log("[map] Indexing mapped files", JSON.stringify(paths, null, 2));

        if (paths) {
            await Promise.all(
                Object.keys(paths).map(path => {
                    if (path !== "*" && paths[path].length === 1) {
                        this.indexPath(path, paths[path][0]);
                    }
                })
            );
        }
    }

    private async indexPath(path: string, target: string): Promise<void> {
        // Special case simple rewrites to index files
        log("[map] Indexing path: ", path, target);

        try {
            if (!path.includes("*")) {
                await this.context.getSourceFileFor(target);
                const sourceFile = await this.context.updateSourceFileFor(
                    target
                ); // FIXME: Why?????????
                log("[map] Found non-* including path: ", path);
                const indexer = new FileIndexer(
                    getModuleName(path) as ModuleName,
                    sourceFile,
                    fileName => this.context.getSourceFileFor(fileName)
                );
                this.context.moduleIndexerMap.set(path, indexer);
                return indexer.indexFile();
            }
        } catch (e) {
            log("[map]", e);
        }
    }

    private async indexProjectFiles(): Promise<void> {
        const files = await this.project.getFileNames();

        log(JSON.stringify(files, null, 2));
        const promises = files.map(file =>
            this.context.getSourceFileFor(file).then(() => {
                log("Indexing: ", file);
                return this.context.reloadFile(file);
            })
        );

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

    private async indexDefinitionFile(
        moduleName: string,
        filename: string
    ): Promise<void> {
        await this.context.getSourceFileFor(filename);
        const sourceFile = await this.context.updateSourceFileFor(filename); // FIXME: Why?????????
        log("Indexing definition file:", sourceFile.fileName, moduleName);
        const indexer = new FileIndexer(
            getModuleName(moduleName) as ModuleName,
            sourceFile,
            fileName => this.context.getSourceFileFor(fileName)
        );
        this.context.moduleIndexerMap.set(moduleName, indexer);
        return indexer.indexFile();
    }
}
