/* 0-indexed within file */
export interface RegionSpan {
    start: number;
    end: number;
}

export interface SymbolLocation {
    name: string;
    location: {
        filename: string;
        pos?: number;
    };
    type: string;
    default?: boolean;
}

export interface Location {
    line: number;
    offset: number;
}

export interface Extent {
    start: Location;
    end: Location;
}

export interface CodeEdit extends Extent {
    newText: string;
}

export interface CodeEditForFile {
    filename: string; /* absolute */
    edit: CodeEdit;
}
