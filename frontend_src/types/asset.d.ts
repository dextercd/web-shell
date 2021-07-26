// asset/resource imports result in file name strings
declare module "*.wasm" {
    const value: string;
    export default value;
}
