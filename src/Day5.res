type operation = {from: int, to: int, count: int}

let parseInput = async () => {
  let parseStacks = input => {
    let input = input->Js.String2.split("\n")

    let stacks = {
      let stackSize =
        input
        ->Js.Array2.pop
        ->Js.Option.getExn
        ->Js.String2.splitByRe(%re("/\s+/"))
        ->Belt.Array.keepMap(x => x->Js.Option.getExn->Belt.Int.fromString)
        ->Js.Math.maxMany_int

      Array.make(stackSize, [])
    }

    while input->Js.Array2.length > 0 {
      let stackIndex = ref(0)
      let pos = ref(0)

      let input = input->Js.Array2.pop->Js.Option.getExn

      while pos.contents < input->Js.String2.length {
        let box =
          input
          ->Js.String2.slice(~from=pos.contents, ~to_=pos.contents + 4)
          ->Js.String2.trim
          ->Js.String2.charAt(1)

        switch box {
        | "" => ()
        | _ =>
          stacks->Array.setExn(
            stackIndex.contents,
            stacks->Array.getExn(stackIndex.contents)->Js.Array2.concat([box]),
          )
        }

        pos.contents = pos.contents + 4
        stackIndex.contents = stackIndex.contents + 1
      }
    }

    stacks
  }

  let parseOperations = input => {
    input
    ->Js.String2.split("\n")
    ->Belt.Array.keepMap(input => {
      switch %re("/move (\d+) from (\d+) to (\d+)/gm")->Js.Re.exec_(input) {
      | None => None
      | Some(result) => {
          let parseCapturesToInt = index =>
            result
            ->Js.Re.captures
            ->Js.Array2.unsafe_get(index)
            ->Js.Nullable.toOption
            ->Belt.Option.mapWithDefault(-1, x => Belt.Int.fromString(x)->Belt.Option.getExn)

          let count = parseCapturesToInt(1)
          let from = parseCapturesToInt(2) - 1
          let to = parseCapturesToInt(3) - 1

          Some({count, from, to})
        }
      }
    })
  }

  let (stacksInput, operationsInput) = {
    let array = (await Core.fetchInput(~year=2022, ~day=5))->Js.String2.split("\n\n")

    let stacksInput = array->Js.Array2.unsafe_get(0)
    let operationsInput = array->Js.Array2.unsafe_get(1)

    (stacksInput, operationsInput)
  }

  let stacks = parseStacks(stacksInput)
  let operations = parseOperations(operationsInput)

  (stacks, operations)
}

let part1 = async () => {
  let (stacks, operations) = await parseInput()

  operations->Js.Array2.forEach(({from, to, count}) => {
    Array.make(count, ())->Array.forEach(() => {
      let fromStack = stacks->Array.getExn(from)
      let toStack = stacks->Array.getExn(to)
      switch fromStack->Js.Array2.pop {
      | Some(box) => stacks->Array.setExn(to, toStack->Js.Array2.concat([box]))
      | _ => ()
      }
    })
  })

  stacks->Array.map(stack => stack->Array.getExn(stack->Array.length - 1))->Js.Array2.joinWith("")
}

let part2 = async () => {
  let (stacks, operations) = await parseInput()

  operations->Js.Array2.forEach(({from, to, count}) => {
    let fromStack = stacks->Array.getExn(from)
    let toStack = stacks->Array.getExn(to)

    stacks->Array.setExn(
      to,
      toStack->Js.Array2.concat(
        fromStack->Js.Array2.spliceInPlace(
          ~add=[],
          ~pos=fromStack->Js.Array2.length - count,
          ~remove=count,
        ),
      ),
    )
  })

  stacks->Array.map(stack => stack->Array.getExn(stack->Array.length - 1))->Js.Array2.joinWith("")
}
