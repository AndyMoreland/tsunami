import * as ts from "typescript";
import * as fs from "fs";
import * as path from "path";
import log from "./log";
import * as Promise from "bluebird";
import * as globAsync from "glob";

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

    public getDependencyFilenames(): Promise<string[]> {
        return this.getModuleTypingsFilenames();
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
