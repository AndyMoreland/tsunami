import * as Bluebird from "bluebird";
import { Command } from "./Command";
import { CommandInvoker } from "./CommandInvoker";
import { TsunamiContext } from "./Context";
import log from "./log";

export class BenchmarkingCommandInvoker implements CommandInvoker {
    constructor(private delegate: CommandInvoker) {}

    public isInvokableCommand(command: Command): boolean {
        return this.delegate.isInvokableCommand(command);
    }

    public async invoke(context: TsunamiContext, command: Command): Promise<void> {
        const startTime = process.hrtime();
        try {
            await this.delegate.invoke(context, command);
        } finally {
            const diff = process.hrtime(startTime);
            log("[Benchmark]: ", command.command, " completed in ", (diff[0] * 1e9 + diff[1]) / 1e6, " milliseconds");
        }
    }
}
