type rec node = {
  type_: [#directory | #file],
  name: string,
  mutable parent: option<node>,
  mutable size: int,
  children: option<Js.Dict.t<node>>,
}

let parseDirectoryTree = input => {
  let rootNode = {
    type_: #directory,
    name: "/",
    parent: None,
    size: 0,
    children: Some(Js.Dict.fromArray([])),
  }

  let currentPosition = ref(rootNode)

  input
  ->String.trim
  ->Js.String2.split("\n")
  ->Array.forEach(input => {
    let args = input->Js.String2.split(" ")

    switch (args[0]->Option.getExn, args[1]->Option.getExn) {
    | ("$", "cd") =>
      switch args[2]->Option.getExn {
      | "/" => currentPosition := rootNode
      | ".." => currentPosition := currentPosition.contents.parent->Option.getExn
      | directoryName =>
        switch currentPosition.contents.children->Option.getExn->Js.Dict.get(directoryName) {
        | None =>
          Js.Exn.raiseError(
            currentPosition.contents.name ++ "doesn't have " ++ directoryName ++ "directory.",
          )
        | Some(childNode) =>
          switch childNode.type_ {
          | #file => Js.Exn.raiseError(childNode.name ++ "isn't directory node.")
          | #directory => {
              childNode.parent = Some(currentPosition.contents)
              currentPosition := childNode
            }
          }
        }
      }
    | ("$", "ls") => ()
    | ("dir", directoryName) => {
        let childNode = {
          type_: #directory,
          name: directoryName,
          parent: Some(currentPosition.contents),
          size: 0,
          children: Some(Js.Dict.fromArray([])),
        }

        currentPosition.contents.children->Option.getExn->Js.Dict.set(directoryName, childNode)
      }

    | (fileSize, fileName) => {
        let childNode = {
          type_: #file,
          name: fileName,
          parent: Some(currentPosition.contents),
          size: fileSize->Int.fromString->Option.getExn,
          children: None,
        }

        currentPosition.contents.children->Option.getExn->Js.Dict.set(fileName, childNode)
      }
    }
  })

  rootNode
}

let rec getNodeSize = node => {
  switch node.type_ {
  | #file => node.size
  | #directory =>
    node.size =
      node.children
      ->Option.getExn
      ->Js.Dict.values
      ->Array.reduce(0, (acc, childNode) => acc + getNodeSize(childNode))

    node.size
  }
}

let rec getDirectoryNodes = node => {
  switch node.type_ {
  | #file => []
  | #directory =>
    [node]->Array.concat(
      node.children->Option.getExn->Js.Dict.values->Array.flatMap(getDirectoryNodes),
    )
  }
}

let part1 = async () => {
  let rootNode = (await Core.fetchInput(~year=2022, ~day=7))->parseDirectoryTree

  rootNode->getNodeSize->ignore
  rootNode
  ->getDirectoryNodes
  ->Array.keep(node => node.size < 100000)
  ->Array.reduce(0, (acc, node) => acc + node.size)
}

let part2 = async () => {
  // parseInput()
  ()
}
