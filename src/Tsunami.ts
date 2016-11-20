import { ProjectIndexer } from "./ProjectIndexer";
import * as JSONStream from "JSONStream";
import * as Bluebird from "bluebird";
import * as p from "child_process";
import * as es from "event-stream";
import * as fs from "fs";
import * as ts from "typescript";
import { BenchmarkingCommandInvoker } from "./BenchmarkingCommandInvoker";
import { Command, CommandDefinition } from "./Command";
import { CommandInvoker } from "./CommandInvoker";
import { TsunamiContext } from "./Context";
import { FileIndexer } from "./FileIndexer";
import { MutableTsunamiContext } from "./MutableTsunamiContext";
import { getErrorOutputForCommand } from "./Response";
import { SimpleCommandInvoker } from "./SimpleCommandInvoker";
import { InitializedFormatOptions } from "./formatting/FormatOptions";
import { ModuleName } from "./imports/ImportStatement";
import log from "./log";
import { TsProject } from "./tsProject";
import { CallbackFunction, UnknownObject } from "./types";
import { writeOutputToStdOut } from "./utilities/ioUtils";

function parseCommand(data: {[index: string]: any}): Command {
    if (data["command"] !== undefined && data["seq"] !== undefined) {
        return data as Command;
    } else {
        throw new Error("Invalid command format.");
    }
}

export class Tsunami {
    private terminalInvoker: CommandInvoker;
    private nonterminalInvoker: CommandInvoker;
    private documentRegistry: ts.DocumentRegistry;
    private context: TsunamiContext;
    private projectIndexer: ProjectIndexer;

    constructor(
        private tsProject: TsProject,
        private formatOptions: InitializedFormatOptions,
        terminalCommandDefinitions: CommandDefinition<any, any>[] = [],
        nonterminalCommandDefinitions: CommandDefinition<any, any>[] = [],
        context?: TsunamiContext
    ) {
        this.terminalInvoker = new BenchmarkingCommandInvoker(new SimpleCommandInvoker(terminalCommandDefinitions));
        this.nonterminalInvoker = new BenchmarkingCommandInvoker(new SimpleCommandInvoker(nonterminalCommandDefinitions));
        this.documentRegistry = ts.createDocumentRegistry(true);
        this.context = context || new MutableTsunamiContext(
            this.tsProject,
            this.formatOptions,
            writeOutputToStdOut,
            this.documentRegistry
        );
        this.projectIndexer = new ProjectIndexer(tsProject, this.context);
    }

    public async initialize(): Promise<void> {
        log("initializing");
        await this.buildRiggedTsServerProcess();
        await this.buildInitialProjectIndex();
        log("initialized");
    }

    public getContext(): TsunamiContext {
        return this.context;
    }

    public buildRiggedTsServerProcess() {
        let tsserver: p.ChildProcess = p.spawn("tsserver");

        process.stdin.resume();
        process.stdin
            .pipe(JSONStream.parse(undefined))
            .pipe(es.map(this.processPotentialTsunamiCommand))
            .pipe(tsserver.stdin);

        tsserver.stdout.pipe(process.stdout);
        tsserver.stdout
            .pipe(es.map((data: any, cb: CallbackFunction<any>) => {
                log(data);
                cb(null, data);
            }));
    }

    public async buildInitialProjectIndex(): Promise<void> {
        await this.projectIndexer.indexProject();
    }

    private processPotentialTsunamiCommand = (data: UnknownObject, cb: CallbackFunction<string>) => {
        // log("Incoming command: ", JSON.stringify(data, null, 2));
        let command = parseCommand(data);

        if (this.terminalInvoker.isInvokableCommand(command)) {
            // log("Processing command with tsunami.");
            try {
                this.terminalInvoker.invoke(this.context, command);
            } catch (e) {
                /* Prevents propagation of command to tsserver.stdin */
                log("Error processing tsunami command: ", JSON.stringify(command, null, 2), e.stack);
                writeOutputToStdOut<string>(getErrorOutputForCommand(command, e));
            }
            cb();
        } else {
            if (this.nonterminalInvoker.isInvokableCommand(command)) {
                // log("Wiretapping command with tsunami.");
                try {
                    this.nonterminalInvoker.invoke(this.context, command);
                } catch (e) {
                    log("Error wiretapping tsunami command: ", JSON.stringify(command, null, 2), e);
                }
            }
            // log("Proxying to tsserver: ", JSON.stringify(command, null, 2));
            /* Pass the re-string-form'd object straight through. */
            cb(null, JSON.stringify(command) + "\n");
        }
    }


}
