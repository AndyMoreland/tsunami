import * as fs from "fs";

const LOGGING_ENABLED = false;

export default function log(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile("/Users/amoreland/tsunami/log.txt", "\n\n" + "[" + process.pid + "]: " + args.join(", "));
    }
}

export function logWithCallback(cb: Function, ...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile("/Users/amoreland/tsunami/log.txt", "\n\n" + "[" + process.pid + "]: " + args.join(", "), cb);
    } else {
        cb();
    }
}

export function logSync(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFileSync("/Users/amoreland/tsunami/log.txt", "\n\n" + "[" + process.pid + "]: " + args.join(", "));
    }
}
