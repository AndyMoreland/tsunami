/// <reference path="../typings/node/node.d.ts" />
import { getExpressionsContainingPoint } from "./ExpressionTree";
import * as Promise from "bluebird";
import { ModuleIndexer } from "./ModuleIndexer";
import { FileIndexer } from "./FileIndexer";
import { DefinitionType, Indexer } from "./Indexer";
import { logSync, logWithCallback, default as log } from "./log";
import { ImportSorter } from "./importSorter";
import { TsProject } from "./tsProject";
import * as JSONStream from "JSONStream";
import * as p from "child_process";
import * as es from "event-stream";
import * as fs from "fs";
import * as ts from "typescript";

const readFilePromise = Promise.promisify(fs.readFile);

export const SYMBOL_LOCATIONS = "SYMBOL_LOCATIONS";
export const ORGANIZE_IMPORTS = "ORGANIZE_IMPORTS";
export const GET_CONTAINING_EXPRESSIONS = "GET_CONTAINING_EXPRESSIONS";
export const RELOAD = "reload";

/* Yay singletons. */
let GLOBAL_DOCUMENT_REGISTRY: ts.DocumentRegistry = ts.createDocumentRegistry(true);
let GLOBAL_TS_PROJECT: TsProject = null;

/* HACK */
process.on("uncaughtException", (err: any) => {
    logWithCallback((e: any, data: any) => process.exit(), err);
});

/* 0-indexed within file */
interface RegionSpan {
    start: number;
    end: number;
}

interface Command {
    command: string;
    seq: number;
    arguments: any;
}

interface FetchSymbolLocationsCommand extends Command {
    arguments: {
        prefix: string;
    };
}

interface OrganizeImportsCommand extends Command {
    arguments: {
        filename: string;
    };
}

interface ReloadCommand extends Command {
    arguments: {
        file: string;
        tmpfile: string;
    };
}

interface GetContainingExpressionsCommand extends Command {
    arguments: {
        file: string;
        line: number;
        offset: number;
    };
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
    type: string;
    default?: boolean;
}

interface FetchSymbolLocationsResponseBody {
    symbolLocations: SymbolLocation[];
};

var fileIndexerMap: { [filename: string]: Indexer } = {};
/* Note: this is getting stomped on. */
var moduleIndexerMap: { [modulename: string]: Indexer } = {};

function parseCommand(data: {[index: string]: any}): Command {
    if (data["command"] !== undefined && data["seq"] !== undefined) {
        return data as Command;
    } else {
        throw new Error("Invalid command format.");
    }
}

function isTsunamiCommand(command: Command): boolean {
    return isFetchSymbolLocationsCommand(command) ||
        isOrganizeImportsCommand(command) ||
        isGetContainingExpressionsCommand(command);
}

function isTsunamiWiretapCommand(command: Command): boolean {
    return isReloadCommand(command);
}

function isFetchSymbolLocationsCommand(command: Command): command is FetchSymbolLocationsCommand {
    return command.command === SYMBOL_LOCATIONS;
}

function isReloadCommand(command: Command): command is ReloadCommand {
    return command.command === RELOAD;
}

function isOrganizeImportsCommand(command: Command): command is OrganizeImportsCommand {
    return command.command === ORGANIZE_IMPORTS;
}

function isGetContainingExpressionsCommand(command: Command): command is GetContainingExpressionsCommand {
    return command.command === GET_CONTAINING_EXPRESSIONS;
}

function writeOutput<T>(response: Response<T>) {
    let output = JSON.stringify(response);
    let outputLength = output.length;

    log(output);

    process.stdout.write("Content-Length: " + outputLength + "\n\n");
    process.stdout.write(output + "\n");

    return Promise.resolve<void>(null);
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
    response.message = "" + error;

    return response;
}

const fileVersionMap: { [filename: string]: number } = {};

function getSourceFileFor(documentRegistry: ts.DocumentRegistry, filename: string, sourceFileName?: string): Promise<ts.SourceFile> {
    return readFilePromise(sourceFileName || filename).then(file => {
        let sourceText = file.toString();
        fileVersionMap[filename] = 0;
        let sourceFile = documentRegistry.acquireDocument(filename,
                                                          GLOBAL_TS_PROJECT.getCompilerOptions(),
                                                          ts.ScriptSnapshot.fromString(sourceText), "" + fileVersionMap[filename]);
        return sourceFile;
    });
}

function updateSourceFileFor(documentRegistry: ts.DocumentRegistry, filename: string, sourceFileName?: string): Promise<ts.SourceFile> {
    if (fileVersionMap[filename] == null || fileVersionMap[filename] === undefined) {
        return getSourceFileFor(documentRegistry, filename, sourceFileName);
    }

    return readFilePromise(sourceFileName || filename).then(file => {
        let sourceText = file.toString();
        fileVersionMap[filename] = fileVersionMap[filename] + 1;
        let sourceFile = documentRegistry.updateDocument(filename,
                                                         GLOBAL_TS_PROJECT.getCompilerOptions(),
                                                         ts.ScriptSnapshot.fromString(sourceText),
                                                         "" + fileVersionMap[filename]);
        return sourceFile;
    });
}

function processFetchSymbolLocations(command: FetchSymbolLocationsCommand): Promise<void> {
    let response: Response<FetchSymbolLocationsResponseBody> = getBlankResponseForCommand(command);
    let symbolLocations: SymbolLocation[] = [];

    Object.keys(fileIndexerMap).forEach(filename => {
        let definitions = fileIndexerMap[filename].getDefinitionIndex();
        Object.keys(definitions).forEach(
            symbolName => {
                let definition = definitions[symbolName];
                let symbolLocation =  {
                    name: symbolName,
                    type: DefinitionType[definition.type],
                    location: {
                        filename: definition.filename,
                        pos: definition.location
                    },
                    default: definition.default
                };
                symbolLocations.push(symbolLocation);
            });
    });

    Object.keys(moduleIndexerMap).forEach(moduleName => {
        let definitions = moduleIndexerMap[moduleName].getDefinitionIndex();
        Object.keys(definitions).forEach(
            symbolName => {
                let definition = definitions[symbolName];
                let symbolLocation = {
                    name: symbolName,
                    type: DefinitionType[definition.type],
                    location: {
                        filename: moduleName,
                        pos: definition.location,
                        isExternalModule: true
                    },
                    default: definition.default
                };
                symbolLocations.push(symbolLocation);
            });
    });

    response.seq = 1;
    response.success = true;
    response.body = {
        symbolLocations: symbolLocations
    };

    return writeOutput(response) as any as Promise<void>;
}

function processOrganizeImportsCommand(command: OrganizeImportsCommand): Promise<void> {
    let response: Response<string> = getBlankResponseForCommand(command);
    response.seq = 1;

    return updateSourceFileFor(GLOBAL_DOCUMENT_REGISTRY, command.arguments.filename).then(sourceFile => {
        let importSorter = new ImportSorter(sourceFile);
        importSorter.sortFileImports((err?: Error) => {
            if (err) { log(err); }
            response.body = "" + err;
            response.success = true;
            writeOutput(response);
        });
    }).catch(e => {
        log(e);
        response.body = "" + e;
        writeOutput(response);
    });
}

function indexDefinitionFile(documentRegistry: ts.DocumentRegistry, moduleName: string, filename: string): Promise<void> {
    return getSourceFileFor(documentRegistry, filename).then(() => {
        return updateSourceFileFor(documentRegistry, filename).then(sourceFile => {
            log("Indexing definition file:", sourceFile.fileName);
            let indexer = new FileIndexer(sourceFile);
            moduleIndexerMap[moduleName] = indexer;
            indexer.indexFile();
        });
    });
}

function indexExternalModule(moduleName: string): Promise<void> {
    moduleIndexerMap[moduleName] = new ModuleIndexer(moduleName);

    return Promise.resolve(null);
}

function reloadFile(documentRegistry: ts.DocumentRegistry, filename: string, tmpfilename?: string): Promise<void> {
    return updateSourceFileFor(documentRegistry, filename, tmpfilename).then(sourceFile => {
        let indexer = new FileIndexer(sourceFile);
        fileIndexerMap[filename] = indexer;
        indexer.indexFile();
    });
}

function processReloadCommand(command: ReloadCommand): Promise<void> {
    return reloadFile(GLOBAL_DOCUMENT_REGISTRY, command.arguments.file, command.arguments.tmpfile);
}

function processGetContainingExpressionsCommand(command: GetContainingExpressionsCommand): Promise<void> {
    const { line, offset, file } = command.arguments;

    return getSourceFileFor(GLOBAL_DOCUMENT_REGISTRY, file).then(sourceFile => {
        const response: Response<RegionSpan[]> = getBlankResponseForCommand(command);
        const position = sourceFile.getPositionOfLineAndCharacter(line, offset);
        const expressions = getExpressionsContainingPoint(sourceFile, position);
        response.success = true;
        response.body = expressions.map(expr => ({ start: expr.getStart(), end: expr.getEnd()}));
        writeOutput(response);
    }).catch(e => {
        log("Error occurred containing getting expressions: ", e, e.stack);
        writeOutput(getErrorOutputForCommand(command, e));
    });
}

function processTsunamiCommand(command: Command): Promise<void> {
    if (isFetchSymbolLocationsCommand(command)) {
        log("Fetching symbols for prefix: ", command.arguments.prefix);
        return processFetchSymbolLocations(command);
    } else if (isOrganizeImportsCommand(command)) {
        log("Organizing imports for file: ", command.arguments.filename);
        return processOrganizeImportsCommand(command);
    } else if (isReloadCommand(command)) {
        log("Reloading: ", command.arguments.file, command.arguments.tmpfile);
        return processReloadCommand(command);
    } else if (isGetContainingExpressionsCommand(command)) {
        log("Getting containing expressions for: ", command.arguments.file,
            command.arguments.line,
            command.arguments.offset);
        return processGetContainingExpressionsCommand(command);
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

log("Attempting to start server.");
let tsProjectPromise = TsProject.constructFromFilename(projectConfig);
tsProjectPromise.then(tsProject => {
    GLOBAL_TS_PROJECT = tsProject;
    tsProject.getFileNames().then(files => {
        let tsserver: p.ChildProcess = p.spawn("node", ["/Users/amoreland/tsunami/node_modules/typescript/lib/tsserver.js"]);

        process.stdin.resume();
        process.stdin
            .pipe(JSONStream.parse(undefined))
            .pipe(es.map(processPotentialTsunamiCommand))
            .pipe(tsserver.stdin);

        tsserver.stdout
            .pipe(es.map((data: any, cb: CallbackFunction<any>) => { log("Response: ", data); cb(null, data); }))
            .pipe(process.stdout);

        let promises = files.map(file => {
            let p = getSourceFileFor(GLOBAL_DOCUMENT_REGISTRY, file).then(() => {
                return reloadFile(GLOBAL_DOCUMENT_REGISTRY, file);
            });
            return p;
        });

        return Promise.all(promises).then(() => {
            log("Finished starting server.");
        });
    });

    try {
        tsProject.getDependencyFilenames().then(deps => {
            log("Dependency typings: ", JSON.stringify(deps, null, 2));

            Object.keys(deps).forEach(dep => {
                /* indexExternalModule(dep); */
                try {
                    fs.accessSync(deps[dep]);
                    let typings = deps[dep];

                    if (typings) {
                        indexDefinitionFile(GLOBAL_DOCUMENT_REGISTRY, dep, typings);
                    }
                } catch (e) {
                    log("Failed to index: ", dep);
                }
            });
        }).catch(error => {
            log("Failed to get dependency filenames.");
            log(error.stack);
        });
    } catch (e) {
        log("Error during boot.");
        log(e.stack);
    }
});
