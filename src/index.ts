/// <reference path="../typings/node/node.d.ts" />

import {ImportSorter} from "./importSorter";
import {FileIndexer} from "./indexer";
import {default as log, logWithCallback} from "./log";
import { TsProject } from "./tsProject";
import * as JSONStream from "JSONStream";
import * as p from "child_process";
import * as es from "event-stream";
import * as fs from "fs";
import * as ts from "typescript";

export const SYMBOL_LOCATIONS = "SYMBOL_LOCATIONS";
export const ORGANIZE_IMPORTS = "ORGANIZE_IMPORTS";
export const RELOAD = "reload";

/* Yay singletons. */
let GLOBAL_DOCUMENT_REGISTRY: ts.DocumentRegistry = ts.createDocumentRegistry(true);
let GLOBAL_TS_PROJECT: TsProject = null;

/* HACK */
process.on('uncaughtException', (err: any) => {
    logWithCallback(err, (e: any, data: any) => process.exit());
});

interface Command {
    command: string;
    seq: number;
    arguments: any;
}

interface FetchSymbolLocationsCommand extends Command {
    arguments: {
        prefix: string;
    }
}

interface OrganizeImportsCommand extends Command {
    arguments: {
        filename: string;
    }
}

interface ReloadCommand extends Command {
    arguments: {
        file: string;
        tmpfile: string;
    }
}

interface CallbackFunction<T> {
    (error?: Error, data?: T): void;
}

interface UnknownObject {
    [index: string]: any;
}

interface Response<T> {
    seq: number;
    type: string;
    command: string;
    request_seq: number;
    success: boolean;
    message?: string;
    body?: T;
}

interface SymbolLocation {
    name: string;
    location: {
        filename: string;
        pos: number;
    };
}

interface FetchSymbolLocationsResponseBody {
    symbolLocations: SymbolLocation[];
};

var fileIndexerMap: { [filename: string]: FileIndexer } = {};

function parseCommand(data: {[index: string]: any}): Command {
    if (data["command"] !== undefined && data["seq"] !== undefined) {
        return data as Command;
    } else {
        throw new Error("Invalid command format.");
    }
}

function isTsunamiCommand(command: Command): boolean {
    return isFetchSymbolLocationsCommand(command) || isOrganizeImportsCommand(command);
}

function isTsunamiWiretapCommand(command: Command): boolean {
    return isReloadCommand(command);
}

function isFetchSymbolLocationsCommand(command: Command): command is FetchSymbolLocationsCommand {
    return command.command == SYMBOL_LOCATIONS;
}

function isReloadCommand(command: Command): command is ReloadCommand {
    return command.command == RELOAD;
}

function isOrganizeImportsCommand(command: Command): command is OrganizeImportsCommand {
    return command.command == ORGANIZE_IMPORTS;
}

function writeOutput<T>(response: Response<T>) {
    let output = JSON.stringify(response);
    let outputLength = output.length;

    log(output);

    process.stdout.write("Content-Length: " + outputLength + "\n\n");
    process.stdout.write(output + "\n");
}

function getBlankResponseForCommand(command: Command): Response<any> {
    return {
        command: command.command,
        request_seq: command.seq,
        success: undefined,
        seq: undefined,
        type: "response"
    };
}

function getErrorOutputForCommand(command: Command, error: Error): Response<string> {
    let response: Response<string> = getBlankResponseForCommand(command);

    response.success = false;
    response.message = ""+error;

    return response;
}

const fileVersionMap: { [filename: string]: number } = {};

function getSourceFileFor(documentRegistry: ts.DocumentRegistry, filename: string, sourceFileName?: string): ts.SourceFile {
    let sourceText = fs.readFileSync(sourceFileName || filename).toString();
    fileVersionMap[filename] = 0;
    let sourceFile = documentRegistry.acquireDocument(filename, GLOBAL_TS_PROJECT.getCompilerOptions(), ts.ScriptSnapshot.fromString(sourceText), ""+fileVersionMap[filename]);
    return sourceFile;
}

function updateSourceFileFor(documentRegistry: ts.DocumentRegistry, filename: string, sourceFileName?: string): ts.SourceFile {
    let sourceText = fs.readFileSync(sourceFileName || filename).toString();
    fileVersionMap[filename] = fileVersionMap[filename] + 1;
    let sourceFile = documentRegistry.updateDocument(filename, GLOBAL_TS_PROJECT.getCompilerOptions(), ts.ScriptSnapshot.fromString(sourceText), ""+Math.random());
    return sourceFile;
}

function processFetchSymbolLocations(command: FetchSymbolLocationsCommand): void {
    let response: Response<FetchSymbolLocationsResponseBody> = getBlankResponseForCommand(command);
    let symbolLocations: SymbolLocation[] = [];

    Object.keys(fileIndexerMap).forEach(filename => {
        let definitions = fileIndexerMap[filename].getDefinitionIndex();
        Object.keys(definitions).forEach(
            symbolName => {
                let definition = definitions[symbolName];
                let symbolLocation =  {
                    name: symbolName,
                    location: {
                        filename: definition.filename,
                        pos: definition.location
                    }
                }
                symbolLocations.push(symbolLocation);
            });
    });

    response.seq = 1;
    response.body = {
        symbolLocations: symbolLocations
    }

    writeOutput(response);
}

function processOrganizeImportsCommand(command: OrganizeImportsCommand): void {
    let response: Response<string> = getBlankResponseForCommand(command);
    response.seq = 1;

    try {
        let sourceFile = updateSourceFileFor(GLOBAL_DOCUMENT_REGISTRY, command.arguments.filename);
        let importSorter = new ImportSorter(sourceFile);
        importSorter.sortFileImports((err?: Error) => {
            if (err) { log(err); }
            response.body = "" + err;
            writeOutput(response);
        });
    } catch (e) {
        log(e);
        response.body = "" + e;
        writeOutput(response);
    }
}

function reloadFile(documentRegistry: ts.DocumentRegistry, filename: string, tmpfilename?: string) {
    let indexer = new FileIndexer(updateSourceFileFor(documentRegistry, filename, tmpfilename));
    /* log(fileIndexerMap, filename); */
    fileIndexerMap[filename] = indexer;
    indexer.indexFile();
    /* let index = indexer.getDefinitionIndex(); */
    // log("Done indexing.");
    /* log(JSON.stringify(index, null, 2)); */
}

function processReloadCommand(command: ReloadCommand): void {
    reloadFile(GLOBAL_DOCUMENT_REGISTRY, command.arguments.file, command.arguments.tmpfile);
}

function processTsunamiCommand(command: Command): void {
    if (isFetchSymbolLocationsCommand(command)) {
        log("Fetching symbols for prefix: ", command.arguments.prefix);
        processFetchSymbolLocations(command);
    } else if (isOrganizeImportsCommand(command)) {
        log("Organizing imports for file: ", command.arguments.filename);
        processOrganizeImportsCommand(command);
    } else if (isReloadCommand(command)) {
        log("Reloading: ", command.arguments.file, command.arguments.tmpfile);
        processReloadCommand(command);
    }
}

function processPotentialTsunamiCommand(data: UnknownObject, cb: CallbackFunction<string>): void {
    // log("Incoming command: ", JSON.stringify(data, null, 2));
    let command = parseCommand(data);
    if (isTsunamiCommand(command)) {
        // log("Processing command with tsunami.");
        try {
            processTsunamiCommand(command);
        } catch (e) {
            /* Prevents propagation of command to tsserver.stdin */
            log("Error processing tsunami command: ", JSON.stringify(command, null, 2), e);
            writeOutput<string>(getErrorOutputForCommand(command, e));
        }

        cb();
    } else {
        if (isTsunamiWiretapCommand(command)) {
            // log("Wiretapping command with tsunami.");
            try {
                processTsunamiCommand(command);
            } catch (e) {
                log("Error wiretapping tsunami command: ", JSON.stringify(command, null, 2), e);
            }
        }
        // log("Proxying to tsserver: ", JSON.stringify(command, null, 2));
        /* Pass the re-string-form'd object straight through. */
        cb(null, JSON.stringify(command) + "\n");
    }
}



let projectConfig = process.cwd();

let tsProjectPromise = TsProject.constructFromFilename(projectConfig);
tsProjectPromise.then(tsProject => {
    GLOBAL_TS_PROJECT = tsProject;
    tsProject.getFileNames().then(files => {
        files.forEach(file => { getSourceFileFor(GLOBAL_DOCUMENT_REGISTRY, file); reloadFile(GLOBAL_DOCUMENT_REGISTRY, file) });

        let tsserver: p.ChildProcess = p.spawn("node", ["/Users/amoreland/tsunami/node_modules/typescript/lib/tsserver.js"]);
        process.stdin.resume();
        log("Finished starting server.");

        process.stdin
               .pipe(JSONStream.parse(undefined))
               .pipe(es.map(processPotentialTsunamiCommand))
               .pipe(tsserver.stdin);

        tsserver.stdout
                .pipe(es.map((data: any, cb: CallbackFunction<any>) => { log("Response: ", data); cb(null, data) }))
                .pipe(process.stdout);

    });
});
