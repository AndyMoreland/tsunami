/// <reference path="../typings/node/node.d.ts" />
import { Tsunami } from "./Tsunami";
import { TsunamiContext } from "./Context";
import { MutableTsunamiContext } from "./MutableTsunamiContext";
import { CommandInvoker } from "./CommandInvoker";
import { ReloadCommandDefinition } from "./commands/ReloadCommand";
import { OrganizeImportsCommandDefinition } from "./commands/OrganizeImportsCommand";
import { FetchSymbolLocationsDefinition } from "./commands/FetchSymbolLocations";
import { UnknownObject, CallbackFunction } from "./types";
import { GetContainingExpressionsDefinition, GetContainingExpressionsCommand } from "./commands/GetContainingExpressions";
import { getErrorOutputForCommand, getBlankResponseForCommand, Response } from "./Response";
import { Command } from "./Command";
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

/* HACK */
process.on("uncaughtException", (err: any) => {
    logWithCallback((e: any, data: any) => process.exit(), err);
});

const terminalCommandDefinitions = [
    new FetchSymbolLocationsDefinition(),
    new OrganizeImportsCommandDefinition(),
    new GetContainingExpressionsDefinition()
];

const nonterminalCommandDefinitions = [
    new ReloadCommandDefinition()
];

let projectConfig = process.cwd();

log("Attempting to start server.");
let tsProjectPromise = TsProject.constructFromFilename(projectConfig)
    .then(project => {
        log("Constructing tsunami");
        const tsunami = new Tsunami(project, terminalCommandDefinitions, nonterminalCommandDefinitions);
        tsunami.initialize();
        log("Done with .initialize");
    });
