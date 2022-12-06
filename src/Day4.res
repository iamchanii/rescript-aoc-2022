type assignment = (int, int)

let parseRowIntoAssignmentTuple = row => {
  row
  ->Js.String2.split(",")
  ->Js.Array2.map(sector => {
    let sectorsAsInt =
      sector->Js.String2.split("-")->Js.Array2.map(x => x->Belt.Int.fromString->Belt.Option.getExn)

    (
      sectorsAsInt->Belt.Array.get(0)->Belt.Option.getExn,
      sectorsAsInt->Belt.Array.get(1)->Belt.Option.getExn,
    )
  })
}

let isContains = (leftAssignment: assignment, rightAssignment: assignment) => {
  let (leftMin, leftMax) = leftAssignment
  let (rightMin, rightMax) = rightAssignment

  Belt.Array.range(rightMin, rightMax)->Js.Array2.every(number =>
    leftMin <= number && leftMax >= number
  )
}

let isPairsFullyContains = (leftAssignment, rightAssignment) => {
  isContains(leftAssignment, rightAssignment) || isContains(rightAssignment, leftAssignment)
}

let parseInput = () => {
  Core.fetchInput(~year=2022, ~day=4)->thenResolve(input => {
    input->Js.String2.split("\n")->Js.Array2.map(parseRowIntoAssignmentTuple)
  })
}

let part1 = async () => {
  let input = await parseInput()

  input
  ->Belt.Array.keepMap(row => {
    let left = row->Js.Array2.unsafe_get(0)
    let right = row->Js.Array2.unsafe_get(1)

    switch isPairsFullyContains(left, right) {
    | false => None
    | true => Some(0)
    }
  })
  ->Belt.Array.length
}

let isIntersect = (leftAssignment: assignment, rightAssignment: assignment) => {
  let (leftMin, leftMax) = leftAssignment
  let (rightMin, rightMax) = rightAssignment

  leftMax - rightMin >= 0 && rightMax - leftMin >= 0
}

let part2 = async () => {
  let input = await parseInput()

  input
  ->Belt.Array.keepMap(row => {
    let left = row->Js.Array2.unsafe_get(0)
    let right = row->Js.Array2.unsafe_get(1)

    switch isIntersect(left, right) {
    | false => None
    | true => Some(0)
    }
  })
  ->Belt.Array.length
}
