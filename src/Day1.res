let parseInput = () => {
  Core.readInput()
  ->Js.String2.split("\n\n")
  ->Js.Array2.map(list => list->Js.String2.split("\n")->Js.Array2.map(Belt.Int.fromString))
  ->Js.Array2.map(list => list->Js.Array2.reduce((prev, curr) => prev + curr->Js.Option.getExn, 0))
}

let part1 = () => {
  parseInput()->Js.Math.maxMany_int
}

let part2 = () => {
  parseInput()
  ->Js.Array2.sortInPlaceWith((a, b) => b - a)
  ->Js.Array2.slice(~start=0, ~end_=3)
  ->Js.Array2.reduce((prev, curr) => prev + curr, 0)
}
