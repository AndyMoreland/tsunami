/// <reference path="../typings/node/node.d.ts" />
import { Tsunami } from "./Tsunami";
import { FetchSymbolLocationsDefinition } from "./commands/FetchSymbolLocations";
import { GetContainingExpressionsDefinition } from "./commands/GetContainingExpressions";
import { GetContainingScopesDefinition } from "./commands/GetContainingScopes";
import { OrganizeImportsCommandDefinition } from "./commands/OrganizeImportsCommand";
import { ReloadCommandDefinition } from "./commands/ReloadCommand";
import { logWithCallback, default as log } from "./log";
import { TsProject } from "./tsProject";

/* HACK */
process.on("uncaughtException", (err: any) => {
    logWithCallback((e: any, data: any) => process.exit(), err);
});

const terminalCommandDefinitions = [
    new FetchSymbolLocationsDefinition(),
    new OrganizeImportsCommandDefinition(),
    new GetContainingExpressionsDefinition(),
    new GetContainingScopesDefinition()
];

const nonterminalCommandDefinitions = [
    new ReloadCommandDefinition()
];

let projectConfig = process.cwd();

log("Attempting to start server.");
TsProject.constructFromFilename(projectConfig)
    .then(project => {
        log("Constructing tsunami");
        const tsunami = new Tsunami(project, terminalCommandDefinitions, nonterminalCommandDefinitions);
        tsunami.initialize();
        log("Done with .initialize");
    });
