export interface FormatOptions {
    indentSize?: number;
}

export interface InitializedFormatOptions {
    indentSize: number;
}

const DefaultFormatOptions: InitializedFormatOptions = {
    indentSize: 2
};

export function buildFormatOptions(overrides: FormatOptions = {}): InitializedFormatOptions {
    return Object.assign(
        {},
        DefaultFormatOptions,
        overrides
    );
}
