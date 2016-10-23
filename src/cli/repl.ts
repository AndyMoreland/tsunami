#!/usr/bin/env node

import * as fs from "fs";
import * as ts from "typescript";
import * as path from "path";
import * as readline from "readline";
import { getNodesContainingPoint } from "../utilities/languageUtilities";
import { TsunamiContext } from "../Context";
import * as tsu from "../index";

async function startRepl() {
    const projectRoot = process.argv[2];
    console.log("Starting in: ", projectRoot);
    const settings = JSON.parse(fs.readFileSync(path.join(projectRoot, "tsconfig.json")).toString());
    const tsunami = new tsu.Tsunami(
        new tsu.TsProject(projectRoot, settings)
    );

    await tsunami.buildInitialProjectIndex();

    console.log("Built initial index.");

    return tsunami;
}

async function processSearch(context: TsunamiContext, query: string) {
    console.log("Searching symbols matchings: ", query);

    const results = await context.getMatchingSymbols(query);
    results.slice(0, 10).map(it => it.text).forEach(it => {
        console.log(it);
    });
}

async function processTypeQuery(context: TsunamiContext, fileName: string, pos: number) {
    fileName = path.resolve(fileName);
    console.log("Examining position ", pos, " in ", fileName);
    const sourceFile = await context.getSourceFileFor(fileName);
    const nodes = getNodesContainingPoint(sourceFile, pos);
    const program = await context.getProgram();
    const checker = program.getTypeChecker();
    nodes.forEach(node => {
        try {
            const nodeType = checker.getTypeAtLocation(node);
            console.log("Got node kind: ", ts.SyntaxKind[node.kind], " with type: ", checker.typeToString(nodeType));
            console.log("Properties:", nodeType.getProperties().map(sym => checker.symbolToString(sym)));
        } catch (e) {
            console.error("Couldn't get node type. Error: ", e);
        }
    });
}

async function processGetDiagnostics(context: TsunamiContext, fileName: string) {
    console.log("Getting diagnostics for file: ", path.resolve(fileName));
    const program = await context.getProgram();
    const diags = program.getSemanticDiagnostics();
    console.log(diags);
}

async function processCommand(context: TsunamiContext, input: string) {
    const [commandName, ...args] = input.split(" ");

    switch (commandName) {
        case "c": return processSearch(context, args[0]);
        case "t": return processTypeQuery(context, args[0], parseInt(args[1], 10));
        case "d": return processGetDiagnostics(context, args[0]);
    }
}

async function main() {
    console.log("Starting main.");
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    const prompt = (query: string) => new Promise<string>((resolve, reject) => rl.question(query, resolve));

    const context = (await startRepl()).getContext();

    while (true) {
        const input = await prompt("> ");
        const start = process.hrtime();
        try {
            await processCommand(context, input);
        } catch (e) {
            console.error("While processing command, got error: ", e, e.stack);
        }
        const diff = process.hrtime(start);
        console.log(`[${(diff[0] * 1e9 + diff[1]) / 1e6} milliseconds]`);
    }
}

main();
