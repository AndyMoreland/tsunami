import { ParseConfigHost } from "./typescript-tools/ParseConfigHost";
import * as Bluebird from "bluebird";
import * as findRoot from "find-root";
import * as fs from "fs";
import * as globAsync from "glob";
import * as path from "path";
import * as ts from "typescript";
import log from "./log";

const readFilePromise = Bluebird.promisify(fs.readFile);

export class TsProject {
    public static async fromRootDir(tsconfigFolder: string): Bluebird<TsProject> {
        const filename = path.join(tsconfigFolder, "tsconfig.json");
        const parseConfigHost = new ParseConfigHost();
        const data = await readFilePromise(filename);

        const config = () => ts.parseJsonConfigFileContent(
            JSON.parse("" + data), parseConfigHost, tsconfigFolder
        );

        return new TsProject( tsconfigFolder, config);
    }

    constructor(private projectRoot: string,
                private getTsconfig: () => ts.ParsedCommandLine) {}

    public getRoot(): string {
        return this.projectRoot;
    }

    private async getProjectFilenames(): Bluebird<string[]> {
        return this.getTsconfig().fileNames;
    }

    private async getTypingsFileForModuleName(baseDir: string, moduleName: string): Promise<string> {
        const nodeModulesFolder = path.join(baseDir, "node_modules");
        const moduleFolder = path.join(nodeModulesFolder, moduleName);
        const modulePackage = path.join(moduleFolder, "package.json");
        const data = await readFilePromise(modulePackage);
        const typingsFile = JSON.parse(data.toString()).typings || "index.d.ts";
        return path.join(moduleFolder, typingsFile);
    }

    public async getAllFilenames(): Bluebird<string[]> {
        return this.getProjectFilenames();
    }

    public async getFileNames(): Bluebird<string[]> {
        return this.getProjectFilenames();
    }

    public async getDependencyFilenames(): Promise<{ [index: string]: string }> {
        const packageJsonFolder = findRoot(this.projectRoot);
        const packageJsonFilename = path.join(packageJsonFolder, "package.json");
        log("Searching for dependencies in: ", packageJsonFilename);
        const data = await readFilePromise(packageJsonFilename);
        const packageJson = JSON.parse(data.toString());
        log(Object.keys(packageJson.dependencies));

        const deps = packageJson.dependencies as { [index: string]: string };
        const response: { [index: string]: string } = {};
        await Bluebird.all(Object.keys(deps).map(
            dep => this.getTypingsFileForModuleName(packageJsonFolder, dep)
                .then(typingsFile => response[dep] = typingsFile)
        ));

        return response;
    }

    public getCompilerOptions(): ts.CompilerOptions {
        return this.getTsconfig().options;
    }
}
