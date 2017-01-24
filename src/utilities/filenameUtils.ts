/**
 * Strip trailing "/index.ts" from folder indexes.
 */
export function fileNameToModuleSpecifier(fileName: string): string {
    const match = fileName.match(/(.*)\/index\.tsx?$/);
    if (match) {
        return match[1];
    } else {
        return fileName;
    }
}

export function pathToModuleSpecifier(path: string): string {
    return path.replace(/\\/g, "/");
}
