import { DeferredResponse } from "../utilities/deferred";
import { RefactoringContext, UserChoice } from "./RefactoringContext";

export class MockRefactoringContext implements RefactoringContext {
    private textResponse = DeferredResponse.empty<string>();
    private choiceResponse = DeferredResponse.empty<UserChoice<any>>();

    promptForText(prompt: string, initial: string | undefined): Promise<string | undefined> {
        return this.textResponse.promise;
    }

    promptChoice<T>(choices: UserChoice<T>[]): Promise<UserChoice<T> | undefined> {
        return this.choiceResponse.promise;
    }

    respondWithText(response: string): void {
        this.textResponse.resolve(response);
        this.textResponse = DeferredResponse.empty<string>();
    }

    respondWithChoice<T>(choice: UserChoice<T>): void {
        this.choiceResponse.resolve(choice);
        this.choiceResponse = DeferredResponse.empty<UserChoice<T>>();
    }
}
