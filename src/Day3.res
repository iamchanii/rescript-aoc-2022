exception NotFound
exception InvalidInput

let splitRow = row => {
  let length = row->Js.String2.length
  let compartmentSize = length / 2

  (
    row->Js.String2.slice(~from=0, ~to_=compartmentSize),
    row->Js.String2.sliceToEnd(~from=compartmentSize),
  )
}

let findAppearsInBothCompartments = (source, compartments) => {
  let result =
    source
    ->Js.String2.split("")
    ->Js.Array2.find(element =>
      compartments->Js.Array2.every(compartment => element->Js.String.includes(compartment))
    )

  switch result {
  | Some(x) => x
  | _ => raise(NotFound)
  }
}

let calculatePriority = element => {
  let charCode = element->Js.String2.charCodeAt(0)
  let priority = {
    if charCode >= 65. && charCode <= 90. {
      charCode -. 38.
    } else if charCode >= 97. && charCode <= 122. {
      charCode -. 96.
    } else {
      raise(InvalidInput)
    }
  }

  priority->Belt.Int.fromFloat
}

let part1 = () => {
  Core.readInput()
  ->Js.String2.split("\n")
  ->Js.Array2.map(row => {
    let (left, right) = row->splitRow
    findAppearsInBothCompartments(left, [right])
  })
  ->Js.Array2.reduce((result, element) => result + element->calculatePriority, 0)
}

let part2 = () => {
  let list = Core.readInput()->Js.String2.split("\n")

  let rec calculate = (arr, result: int) => {
    if arr->Js.Array2.length < 3 {
      result
    } else {
      let group = list->Js.Array2.spliceInPlace(~pos=0, ~remove=3, ~add=[])
      let shortest =
        group
        ->Belt.SortArray.stableSortBy((a, b) => a->Js.String.length - b->Js.String.length)
        ->Belt.Array.get(0)
        ->Belt.Option.getWithDefault("")

      calculate(arr, result + findAppearsInBothCompartments(shortest, group)->calculatePriority)
    }
  }

  calculate(list, 0)
}
