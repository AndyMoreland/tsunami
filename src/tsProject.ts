import * as ts from "typescript";
import * as fs from "fs";
import * as path from "path";
import log from "./log";
import * as Promise from "bluebird";
import * as globAsync from "glob";
import * as findRoot from "find-root";

const glob = Promise.promisify(globAsync) as (pattern: string, options?: any) => Promise<string[]>;

export class TsProject {
    constructor(private projectRoot: string,
                private tsconfig: any) {}

    private getProjectFilenames(): Promise<string[]> {
        return glob(path.join(this.projectRoot, "src", "**/*.@(ts|tsx)"));
    }

    private getModuleTypingsFilenames(): Promise<string[]> {
        return glob(path.join(this.projectRoot, "node_modules", "**/*.d.ts"));
    }

    public getFileNames(): Promise<string[]> {
        return this.getProjectFilenames();
    }

    private getTypingsFileForModuleName(baseDir: string, moduleName: string): Promise<string> {
        let nodeModulesFolder = path.join(baseDir, "node_modules");
        let moduleFolder = path.join(nodeModulesFolder, moduleName);
        let modulePackage = path.join(moduleFolder, "package.json");

        return Promise.promisify(fs.readFile)(modulePackage).then(data => {
            let typingsFile = JSON.parse(data.toString()).typings || "index.d.ts";
            return path.join(moduleFolder, typingsFile);
        });
    }

    public getDependencyFilenames(): Promise<{ [index: string]: string }> {
        let packageJsonFolder = findRoot(this.projectRoot);
        let packageJsonFilename = path.join(packageJsonFolder, "package.json");
        log("Searching for dependencies in: ", packageJsonFilename);
        return Promise.promisify(fs.readFile)(packageJsonFilename).then(data => {
            let packageJson = JSON.parse(data.toString());
            log(Object.keys(packageJson.dependencies));

            let deps = packageJson.dependencies as { [index: string]: string };
            let response: { [index: string]: string } = {};
            return Promise.all(Object.keys(deps).map(dep => {
                return this.getTypingsFileForModuleName(packageJsonFolder, dep).then(typingsFile => {
                    response[dep] = typingsFile;
                });
            }))
                .then(() => { return response; });
        });
    }

    public getCompilerOptions(): ts.CompilerOptions {
        return this.tsconfig.compilerOptions as ts.CompilerOptions;
    }

    public static constructFromFilename(tsconfigFolder: string): Promise<TsProject> {
        let filename = path.join(tsconfigFolder, "tsconfig.json");

        return Promise.promisify(fs.readFile)(filename)
            .then(data => {
                let config = JSON.parse("" + data);
                log("Configuration:", JSON.stringify(config, null, 2));

                return new TsProject(tsconfigFolder, config);
            })
            .catch(err => {
                console.error("Couldn't read file: ", err, filename);
                log("Couldn't read file: ", err, filename);
            });
    }
}
