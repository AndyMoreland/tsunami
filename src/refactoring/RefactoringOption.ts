import { TsunamiContext } from "../Context";
import { RefactoringContext } from "./RefactoringContext";
import { CodeEdit, CodeEditGroup } from "../protocol/types";

export interface RefactoringResult {
    localEdits: CodeEdit[];
    workspaceEdits?: CodeEditGroup;
}

export const RefactoringResult = {
    empty() {
        return {
            localEdits: [],
        }
    }
}

export interface RefactoringOption {
    label: string;
    execute(context: RefactoringContext, globalContext: TsunamiContext): Promise<RefactoringResult>;
}
