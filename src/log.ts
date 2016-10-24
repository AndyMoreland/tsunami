import * as fs from "fs";

const TSUNAMI_LOG_FILE = process.env["TSUNAMI_LOG_FILE"];

const LOGGING_ENABLED = TSUNAMI_LOG_FILE != null;

export default function log(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile(TSUNAMI_LOG_FILE, "\n\n" + "[" + process.pid + "]: " + args.join(", "));
    }
}

export function logWithCallback(cb: Function, ...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile(TSUNAMI_LOG_FILE, "\n\n" + "[" + process.pid + "]: " + args.join(", "), cb);
    } else {
        cb();
    }
}

export function logSync(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFileSync(TSUNAMI_LOG_FILE, "\n\n" + "[" + process.pid + "]: " + args.join(", "));
    }
}
