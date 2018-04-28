export function compact<T>(ts: T[]): T[] {
    return ts.filter(x => x != null);
}
