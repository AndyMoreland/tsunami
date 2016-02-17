declare module "resolve" {

  export interface ResolveOpts {
    basedir?: string;
    extensions?: string[];
    readFile?: any;
    readFileSync?: any;
    isFile?: any;
    isFileSync?: any;
    packageFilter?: any;
    paths?: string[];
    moduleDirectory?: string;
  }


  export function resolve (id: string, opts: ResolveOpts, callback: (err: Error, path: string) => void): void;
  export function sync (id: string, opts: ResolveOpts): string;
  export function isCore (id: string): boolean;
}
