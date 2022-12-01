module Url = {
  @module("url") external fileURLToPath: string => string = "fileURLToPath"
}

@val external importMetaUrl: string = "import.meta.url"
let __filename = Url.fileURLToPath(importMetaUrl)
let __dirname = __filename->Node.Path.dirname

let readInput = () => {
  Node.Path.join([__dirname, "input.txt"])->Node.Fs.readFileAsUtf8Sync
}
