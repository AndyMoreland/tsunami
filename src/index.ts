/// <reference path="../typings/node/node.d.ts" />

import {ImportSorter} from "./importSorter";
import {FileIndexer} from "./indexer";
import * as JSONStream from "JSONStream";
import * as p from "child_process";
import * as es from "event-stream";
import * as fs from "fs";
import * as ts from "typescript";
import log from './log';

export const SYMBOL_LOCATIONS = "SYMBOL_LOCATIONS";
export const ORGANIZE_IMPORTS = "ORGANIZE_IMPORTS";
export const RELOAD = "reload";

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

interface FetchSymbolLocationsResponseBody {};

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

function getSourceFileFor(filename: string, sourceFileName?: string): ts.SourceFile {
  return ts.createSourceFile(filename, fs.readFileSync(sourceFileName || filename).toString(), ts.ScriptTarget.ES5, true);
}

function processFetchSymbolLocations(command: FetchSymbolLocationsCommand): void {
  let response: Response<FetchSymbolLocationsResponseBody> = getBlankResponseForCommand(command);
  response.seq = 1;
  response.body = "hi";

  writeOutput(response);
}

function processOrganizeImportsCommand(command: OrganizeImportsCommand): void {
  let response: Response<string> = getBlankResponseForCommand(command);
  response.seq = 1;

  try {
    let sourceFile = getSourceFileFor(command.arguments.filename);
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

function processReloadCommand(command: ReloadCommand): void {
  let indexer = new FileIndexer(getSourceFileFor(command.arguments.file, command.arguments.tmpfile));
  indexer.indexFile();
  let index = indexer.getDefinitionIndex();
  log("Done indexing.");
  log(JSON.stringify(index, null, 2));
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
  try {
    let command = parseCommand(data);
    if (isTsunamiCommand(command)) {
      // log("Processing command with tsunami.");
      processTsunamiCommand(command);
      /* Prevents propagation of command to tsserver.stdin */
      cb();
    } else {
      if (isTsunamiWiretapCommand(command)) {
        // log("Wiretapping command with tsunami.");
        processTsunamiCommand(command);
      }
      // log("Proxying to tsserver: ", JSON.stringify(command, null, 2));
      /* Pass the re-string-form'd object straight through. */
      cb(null, JSON.stringify(command) + "\n");
    }
  } catch (e) {
    log(e);
    cb(e);
  }
}

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
