import * as fs from "fs";

export default function log(...args: any[]): void {
  fs.appendFile("/Users/amoreland/tsunami/log.txt", "\n\n" + args.join(", "));
}
