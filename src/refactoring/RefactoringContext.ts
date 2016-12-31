export interface UserChoice<T> {
    id: string;
    label: string;
    data: T;
}

export interface RefactoringContext {
    promptForText(prompt: string, initial?: string): Promise<string | undefined>;
    promptChoice<T>(choices: UserChoice<T>[]): Promise<UserChoice<T> | undefined>;
}
