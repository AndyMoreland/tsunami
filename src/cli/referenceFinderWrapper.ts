import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import log from "../log";
import { ReferenceFinder } from "../referenceFinder";
import { TsProject } from "../tsProject";

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(
        filename,
        fs.readFileSync(filename).toString(),
        ts.ScriptTarget.ES5,
        true
    );
}

let args = process.argv.slice(2);
let [moduleName] = args.map(arg => path.resolve(arg));

TsProject.fromRootDir("./").then(tsproject => {
    // console.log("Looking for [", moduleName, "] in [", filename, "]");

    // let indexer = new ImportIndexer(getSourceFileFor(filename));
    // console.log("Modules are: ", indexer.getImportedModules());
    // console.log("Found?", indexer.importsModule(moduleName));

    let refFinder = new ReferenceFinder(tsproject, getSourceFileFor);
    refFinder.findModulesImportingModule(moduleName).then(files => {
        log("Found: ", files, " importing requested module.");
    });
});
