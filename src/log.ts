import * as fs from "fs";
import { NoParamCallback } from "fs";
import * as winston from "winston";

const TSUNAMI_LOG_FILE = process.env["TSUNAMI_LOG_FILE"];

const LOGGING_ENABLED = TSUNAMI_LOG_FILE != null;

export const logger = winston.createLogger({
    level: "info",
    format: winston.format.json(),
    transports: [
        new winston.transports.File({
            filename: TSUNAMI_LOG_FILE + ".error",
            level: "warn"
        }),
        new winston.transports.File({ filename: TSUNAMI_LOG_FILE })
    ]
});

export default function log(...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile(
            TSUNAMI_LOG_FILE!,
            "\n\n" + "[" + process.pid + "]: " + args.join(", "),
            () => {
                /* do nothing */
            }
        );
        logger.info(args.join(", "), { pid: process.pid });
    }
}

export function logWithCallback(cb: NoParamCallback, ...args: any[]): void {
    if (LOGGING_ENABLED) {
        fs.appendFile(
            TSUNAMI_LOG_FILE!,
            "\n\n" + "[" + process.pid + "]: " + args.join(", "),
            () => {
                /* do nothing */
            }
        );
        console.log(...args);
        logger.info(args.join(", "), { pid: process.pid });
        cb(null);
    } else {
        cb(null);
    }
}

export function logSync(...args: any[]): void {
    if (LOGGING_ENABLED) {
        console.log(...args);
        logger.info(args.join(", "), { pid: process.pid });
    }
}
