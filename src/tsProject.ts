import * as Bluebird from "bluebird";
import * as findRoot from "find-root";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import log from "./log";
import { ParseConfigHost } from "./typescript-tools/ParseConfigHost";
import { globPromise } from "./utilities/ioUtils";

const readFilePromise = Bluebird.promisify(fs.readFile);

/**
 * Given a `rootModuleName` like "@foo/bar" and a fileName like "quuxModule.d.ts",
 * returns "@foo/bar/quuxModule"
 */
function guessModuleNameForTypingsFile(
    rootModuleName: string,
    fileName: string
): string {
    return path.join(rootModuleName, path.basename(fileName, ".d.ts"));
}

export class TsProject {
    public static async fromRootDir(
        tsconfigFolder: string
    ): Promise<TsProject> {
        const filename = path.join(tsconfigFolder, "tsconfig.json");
        const parseConfigHost = new ParseConfigHost();

        try {
            const data = await readFilePromise(filename);

            const config = ts.parseJsonConfigFileContent(
                JSON.parse("" + data),
                parseConfigHost,
                tsconfigFolder
            );
            return new TsProject(tsconfigFolder, config);
        } catch (e) {
            throw new Error("Failed to initialize project: " + e);
        }
    }

    private noProjectMode: boolean;

    constructor(
        private projectRoot: string,
        private tsconfig: ts.ParsedCommandLine
    ) {
        this.noProjectMode = this.tsconfig == null;
    }

    public getRoot(): string {
        return this.projectRoot;
    }

    private async getProjectFilenames(): Promise<string[]> {
        return this.tsconfig.fileNames;
    }

    /**
     * Returns set of absolute filenames of typings files in `moduleName`.
     * Returns root-level types + the file specified in "typings".
     */
    private async getTypingsFilesForNodeModule(
        baseDir: string,
        moduleName: string
    ): Promise<{
        index: string;
        extraModules: Set<string>;
    }> {
        const nodeModulesFolder = path.join(baseDir, "node_modules");
        const moduleFolder = path.join(nodeModulesFolder, moduleName);
        const modulePackage = path.join(moduleFolder, "package.json");
        const packageJson = await readFilePromise(modulePackage);
        const indexTypingsFile =
            JSON.parse(packageJson.toString()).typings || "index.d.ts";

        const extraModules = await globPromise("*.d.ts", {
            cwd: moduleFolder
        });

        return {
            index: path.join(moduleFolder, indexTypingsFile),
            extraModules: new Set(
                extraModules.map(x => path.join(moduleFolder, x))
            )
        };
    }

    public async getAllFilenames(): Promise<string[]> {
        return this.getProjectFilenames();
    }

    public async getFileNames(): Promise<string[]> {
        return this.getProjectFilenames();
    }

    public async getDependencyFilenames(): Promise<{
        [moduleName: string]: string;
    }> {
        const packageJsonFolder = findRoot(this.projectRoot);
        const packageJsonFilename = path.join(
            packageJsonFolder,
            "package.json"
        );
        log("Searching for dependencies in: ", packageJsonFilename);
        const data = await readFilePromise(packageJsonFilename);
        const packageJson = JSON.parse(data.toString());
        log(Object.keys(packageJson.dependencies));

        const deps = packageJson.dependencies as { [index: string]: string };
        const response: { [index: string]: string } = {};
        await Bluebird.all(
            Object.keys(deps).map(async depName => {
                const result = await this.getTypingsFilesForNodeModule(
                    packageJsonFolder,
                    depName
                );
                response[depName] = result.index;

                for (let typingsFile of result.extraModules) {
                    guessModuleNameForTypingsFile(depName, typingsFile);
                    response[depName] = typingsFile;
                }
            })
        );

        return response;
    }

    public getCompilerOptions(): ts.CompilerOptions {
        return this.tsconfig.options;
    }
}
