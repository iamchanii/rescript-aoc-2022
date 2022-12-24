module NodeFetchPolyfill = {
  type t
  @module("node-fetch") external fetch: t = "default"
  @val external globalThis: 'a = "globalThis"
  globalThis["fetch"] = fetch
}

module Response = {
  type t
  @send external text: t => Promise.t<Js.String.t> = "text"
}

@val @scope("globalThis")
external fetch: (string, 'params) => Promise.t<Response.t> = "fetch"

module NodeUrl = {
  @module("node:url") external fileURLToPath: string => string = "fileURLToPath"
}

@val external importMetaUrl: string = "import.meta.url"
let __filename = NodeUrl.fileURLToPath(importMetaUrl)
let __dirname = __filename->Node.Path.dirname

let fetchInput = (~year: int, ~day: int) => {
  let readSession = () => {
    Node.Path.join([__dirname, "../.session"])->Node.Fs.readFileAsUtf8Sync
  }

  let url =
    "https://adventofcode.com/" ++ year->Int.toString ++ "/day/" ++ day->Int.toString ++ "/input"

  let params = {
    "headers": {
      "cookie": "session=" ++ readSession(),
    },
  }

  fetch(url, params)->then(response => response->Response.text)
}
