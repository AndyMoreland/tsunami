export type FormatOptions = Partial<InitializedFormatOptions>;

export interface InitializedFormatOptions {
    indentSize: number;
    trailingCommaInObjectLiterals: boolean;
}

const DefaultFormatOptions: InitializedFormatOptions = {
    indentSize: 4,
    trailingCommaInObjectLiterals: false
};

export function buildFormatOptions(overrides: FormatOptions = {}): InitializedFormatOptions {
    return {
        ...DefaultFormatOptions,
        ...overrides
    };
}
