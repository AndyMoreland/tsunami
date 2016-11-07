import * as tsu from "@derander/tsunami";
import { VscodeCommand } from "./VscodeCommand";

export class ReindexProjectCommand implements VscodeCommand {
    public commandName = "tsunami.reindexProject";

    constructor(private tsunami: tsu.Tsunami) {}

    public async execute(): Promise<void> {
        await this.tsunami.buildInitialProjectIndex();
    }
}
