import { Command } from "../Command";
import { ReloadCommand, ReloadCommandDefinition } from "./ReloadCommand";

export class TsunamiReloadCommandDefinition extends ReloadCommandDefinition {
    public predicate(command: Command): command is ReloadCommand {
        return command.command === "tsunami_reload";
    }
}
