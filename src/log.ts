import * as fs from "fs";
import { NoParamCallback } from "fs";

const TSUNAMI_LOG_FILE = process.env["TSUNAMI_LOG_FILE"];

const LOGGING_ENABLED = TSUNAMI_LOG_FILE != null;

export default function log(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile(
            TSUNAMI_LOG_FILE!,
            "\n\n" + "[" + process.pid + "]: " + args.join(", "),
            () => {
                /* do nothing */
            }
        );
    }
}

export function logWithCallback(cb: NoParamCallback, ...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile(
            TSUNAMI_LOG_FILE!,
            "\n\n" + "[" + process.pid + "]: " + args.join(", "),
            cb
        );
    } else {
        cb(null);
    }
}

export function logSync(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFileSync(
            TSUNAMI_LOG_FILE!,
            "\n\n" + "[" + process.pid + "]: " + args.join(", ")
        );
    }
}
