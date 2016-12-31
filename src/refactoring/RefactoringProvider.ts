import * as ts from "typescript";
import { RegionSpan } from "../protocol/types";
import { TsunamiContext } from "../Context";
import { Refactoring } from "./Refactoring";

export interface RefactoringProviderResult {
    refactorings: Refactoring[];
}

export interface RefactoringProvider {
    provideRefactorings(document: ts.SourceFile, range: RegionSpan, context: TsunamiContext): Promise<RefactoringProviderResult>;
}
