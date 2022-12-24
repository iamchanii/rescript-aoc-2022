let parseInput = () => {
  Core.fetchInput(~year=2022, ~day=6)
}

let findStartMarker = (datastream, charactersCount) => {
  let count = ref(charactersCount)
  let flag = ref(true)

  while flag.contents {
    let size =
      datastream
      ->Js.String2.slice(~from=count.contents - charactersCount, ~to_=count.contents)
      ->Js.String2.split("")
      ->Belt.Set.String.fromArray
      ->Belt.Set.String.size

    if size === charactersCount {
      flag.contents = false
    } else {
      count.contents = count.contents + 1
    }
  }

  count.contents
}

let part1 = async () => {
  (await parseInput())->findStartMarker(4)
}

let part2 = async () => {
  (await parseInput())->findStartMarker(14)
}
