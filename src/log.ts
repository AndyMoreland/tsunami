import * as fs from "fs";

export default function log(...args: any[]): void {
    fs.appendFile("/Users/amoreland/tsunami/log.txt", "\n\n" + "[" + process.pid + "]: " + args.join(", "));
}

export function logWithCallback(cb: Function, ...args: any[]): void {
    fs.appendFile("/Users/amoreland/tsunami/log.txt", "\n\n" + "[" + process.pid + "]: " + args.join(", "), cb);
}

export function logSync(...args: any[]): void {
    fs.appendFileSync("/Users/amoreland/tsunami/log.txt", "\n\n" + "[" + process.pid + "]: " + args.join(", "));
}
