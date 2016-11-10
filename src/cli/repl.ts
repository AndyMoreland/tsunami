#!/usr/bin/env node

import * as Bluebird from "bluebird";
import * as path from "path";
import * as readline from "readline";
import * as ts from "typescript";
import { MoveSymbolCommandDefinition } from "../commands/MoveSymbolCommand";
import { buildFormatOptions } from "../formatting/FormatOptions";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { ImportStatementType, AbsoluteFilename, ModuleSpecifier, getTypeOfModuleSpecifier } from "../imports/ImportStatement";
import { getNodesContainingPoint } from "../utilities/languageUtilities";
import { TsunamiContext } from "../Context";
import { Definition } from "../Indexer";
import * as tsu from "../index";

const MOVE_SYMBOL = new MoveSymbolCommandDefinition();

process.on("uncaughtException", (err: Error) => {
    console.error(err);
    process.exit(1);
})

async function startRepl() {
    const projectRoot = process.argv[2];
    console.log("Starting in: ", projectRoot);
    const project = await tsu.TsProject.fromRootDir(projectRoot);
    const tsunami = new tsu.Tsunami(project, buildFormatOptions());

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
    console.log("Examining position ", pos, " in ", fileName);
    fileName = path.resolve(fileName);
    const program = await context.getProgram();
    const sourceFile = program.getSourceFile(fileName);
    const nodes = getNodesContainingPoint(sourceFile, pos);
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

async function processMoveSymbol(context: TsunamiContext, fromFileName: string, toFileName: string, symbolName: string) {
    console.log(`Executing [moveSymbol] on "${symbolName}" from "${path.resolve(fromFileName)}" to "${path.resolve(toFileName)}"`);
    const command = {
        command: "MOVE_SYMBOL",
        seq: 0,
        arguments: {
            fromFilename: path.resolve(fromFileName),
            toFilename: path.resolve(toFileName),
            symbolName
        }
    };

    const edits = await MOVE_SYMBOL.processor(context, command);
    console.log(JSON.stringify(edits, null, 2));
}

async function processGetFilenames(context: TsunamiContext) {
    console.log("Getting filenames.");
    const project = context.getProject();
    const filenames = await project.getFileNames();
    console.log(filenames.join("\n"));
}

async function processFindDeadCode(context: TsunamiContext) {
    console.log("Finding dead code.");
    const files = await context.getProject().getFileNames();

    const promises = files.map(async file => {
        const sourceFile = await context.getSourceFileFor(file);
        return ImportBlockBuilder.fromFile(sourceFile).build();
    });

    const importBlocks = await Bluebird.all(promises);
    const usedSymbols: Map<ModuleSpecifier, Set<string>> = new Map<ModuleSpecifier, Set<string>>();

    for (let block of importBlocks) {
        Object.keys(block.importRecords).forEach((specifier: ModuleSpecifier) => {
            const symbols = usedSymbols.has(specifier) ? usedSymbols.get(specifier)! : new Set<string>();
            block.importRecords[specifier].importClause.namedBindings.forEach(x => symbols.add(x.symbolName));
            usedSymbols.set(specifier, symbols);
        });
    }

    const unused: Definition[] = [];

    for (let def of context.getIndexedDefinitions()) {
        const moduleSpecifier = def.moduleSpecifier.replace(/.tsx?/, "") as ModuleSpecifier;
        if (getTypeOfModuleSpecifier(moduleSpecifier) === ImportStatementType.PROJECT_RELATIVE
            && (!usedSymbols.has(moduleSpecifier) || !usedSymbols.get(moduleSpecifier)!.has(def.text!))) {
            console.log(moduleSpecifier, def.text);
            unused.push(def);
        }
    }
}

async function processImplementInterface(context: TsunamiContext, filename: string, position: number) {
    console.log("Implement interface at location ", position, " in file ", filename);
    const program = await context.getProgram();
    const sourceFile = await program.getSourceFile(filename);
    const containingNodes = getNodesContainingPoint(sourceFile, position);
    const checker = program.getTypeChecker();

    const expressions = containingNodes.filter(
        node => node.kind === ts.SyntaxKind.ExpressionWithTypeArguments
    ) as ts.ExpressionWithTypeArguments[];

    if (expressions.length !== 1) {
        return null;
    }

    const node = expressions[0];
    console.log("Children kinds: ", node.getChildren().map(child => ts.SyntaxKind[child.kind]));
    const props = checker.getTypeAtLocation(node).getProperties();

    console.log("Names: ");
    console.log(props.map(
        prop => prop.getName()
    ));

    console.log("First declaration types: ");
    console.log(props.map(
        prop => checker.typeToString(checker.getTypeAtLocation(prop.getDeclarations()[0]))
    ));

    console.log("Call signatures: ");
    console.log(props.map(
        prop => checker.getTypeAtLocation(prop.getDeclarations()[0]).getCallSignatures()[0].getParameters().map(
            param => [param.getName(), checker.typeToString(checker.getTypeAtLocation(param.getDeclarations()[0]))]
        )
    ));

    return null;
}

async function processCommand(context: TsunamiContext, commandLineArgs: string[]) {
    const start = process.hrtime();
    try {
        const [commandName, ...args] = commandLineArgs;
        switch (commandName) {
        case "c": await processSearch(context, args[0]); break;
        case "t": await processTypeQuery(context, args[0], parseInt(args[1], 10)); break;
        case "d": await processGetDiagnostics(context, args[0]); break;
        case "m": await processMoveSymbol(context, args[0], args[1], args[2]); break;
        case "f": await processGetFilenames(context); break;
        case "i": await processImplementInterface(context, args[0], parseInt(args[1], 10)); break;
        case "dc": await processFindDeadCode(context); break;
        default: console.error("Unknown command: " + commandName);
        }
    } catch (e) {
        console.error("While processing command, got error: ", e, e.stack);
    }
    const diff = process.hrtime(start);
    console.log(`[${(diff[0] * 1e9 + diff[1]) / 1e6} milliseconds]`);
}

async function main() {
    console.log("Starting main.");
    const context = (await startRepl()).getContext();

    if (process.argv.length > 3) {
        console.log("Running immediately.");
        const args = process.argv.slice(3);
        await processCommand(context, args);
    } else {
        console.log("Starting repl.");
        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout
        });
        const prompt = (query: string) => new Promise<string>((resolve, reject) => rl.question(query, resolve));
        while (true) {
            const input = await prompt("> ");
            await processCommand(context, input.split(" "));
        }
    }
}

main().catch(e => {
    console.error("Failed: ", e);
});
