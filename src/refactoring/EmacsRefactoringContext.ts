import { TsunamiContext } from "../Context";
import { UserChoice, RefactoringContext } from "./RefactoringContext";


export class EmacsRefactoringContext implements RefactoringContext {
    constructor(private context: TsunamiContext) {}

    promptForText(prompt: string, initial: string | undefined): Promise<string | undefined> {
        /* TODO: Autogenerated method stub. */
    }

    promptChoice<T>(choices: UserChoice<T>[]): Promise<UserChoice<T> | undefined> {
        /* TODO: Autogenerated method stub. */
    }
}