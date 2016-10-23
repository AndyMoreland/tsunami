import * as Bluebird from "bluebird";
import * as findRoot from "find-root";
import * as fs from "fs";
import * as globAsync from "glob";
import * as path from "path";
import * as ts from "typescript";
import log from "./log";

const glob = Bluebird.promisify(globAsync) as (pattern: string, options?: any) => Bluebird<string[]>;

export class TsProject {
    constructor(private projectRoot: string,
                private tsconfig: any) {}

    public getRoot() {
        return this.projectRoot;
    }

    private getProjectFilenames(): Bluebird<string[]> {
        return glob(path.join(this.projectRoot, "src", "**/*.@(ts|tsx)"));
    }

    public getAllFilenames(): Bluebird<string[]> {
        return glob(path.join(this.projectRoot, "**/*.@(ts|tsx)"));
    }

    public getFileNames(): Bluebird<string[]> {
        return this.getProjectFilenames();
    }

    private getTypingsFileForModuleName(baseDir: string, moduleName: string): Bluebird<string> {
        let nodeModulesFolder = path.join(baseDir, "node_modules");
        let moduleFolder = path.join(nodeModulesFolder, moduleName);
        let modulePackage = path.join(moduleFolder, "package.json");

        return Bluebird.promisify(fs.readFile)(modulePackage).then(data => {
            let typingsFile = JSON.parse(data.toString()).typings || "index.d.ts";
            return path.join(moduleFolder, typingsFile);
        });
    }

    public getDependencyFilenames(): Bluebird<{ [index: string]: string }> {
        let packageJsonFolder = findRoot(this.projectRoot);
        let packageJsonFilename = path.join(packageJsonFolder, "package.json");
        log("Searching for dependencies in: ", packageJsonFilename);
        return Bluebird.promisify(fs.readFile)(packageJsonFilename).then(data => {
            let packageJson = JSON.parse(data.toString());
            log(Object.keys(packageJson.dependencies));

            let deps = packageJson.dependencies as { [index: string]: string };
            let response: { [index: string]: string } = {};
            return Bluebird.all(Object.keys(deps).map(dep => {
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

    public static constructFromFilename(tsconfigFolder: string): Bluebird<TsProject> {
        let filename = path.join(tsconfigFolder, "tsconfig.json");

        return Bluebird.promisify(fs.readFile)(filename)
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
