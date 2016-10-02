export interface Indexer {
    getDefinitionIndex(): DefinitionIndex;
}

export interface DefinitionIndex {
    [symbolName: string]: Definition;
}

export interface Definition {
    text: string | undefined;
    location?: number;
    filename: string;
    type: DefinitionType;
    default: boolean;
};

export enum DefinitionType {
    FUNCTION, EXPORTED_VAR, INTERFACE, ENUM, CLASS, TYPE, MODULE, EXTERNAL_MODULE
};
