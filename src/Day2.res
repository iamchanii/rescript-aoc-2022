exception InvalidInput

type response = Rock | Scissors | Paper

let parseOpponentResponse = input => {
  switch input {
  | "A" => Rock
  | "B" => Paper
  | "C" => Scissors
  | _ => raise(InvalidInput)
  }
}

let parseOwnResponse = input => {
  switch input {
  | "X" => Rock
  | "Y" => Paper
  | "Z" => Scissors
  | _ => raise(InvalidInput)
  }
}

let parseOwnResponseByOpponentResponse = (opponents, input) => {
  switch input {
  | "X" =>
    switch opponents {
    | Rock => Scissors
    | Scissors => Paper
    | Paper => Rock
    }
  | "Y" => opponents
  | "Z" =>
    switch opponents {
    | Rock => Paper
    | Scissors => Rock
    | Paper => Scissors
    }
  | _ => raise(InvalidInput)
  }
}

let calculateResultScore = round => {
  switch round {
  | (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3
  | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 6
  | _ => 0
  }
}

let calculateInputScore = input => {
  switch input {
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  }
}

let calculate = rows =>
  rows->Js.Array2.reduce(
    (result, (opponents, own)) =>
      result + calculateResultScore((opponents, own)) + calculateInputScore(own),
    0,
  )

let part1 = () => {
  Core.readInput()
  ->Js.String2.split("\n")
  ->Js.Array2.map(row => {
    let array = row->Js.String2.split(" ")
    (array[0]->parseOpponentResponse, array[1]->parseOwnResponse)
  })
  ->calculate
}

let part2 = () => {
  Core.readInput()
  ->Js.String2.split("\n")
  ->Js.Array2.map(row => {
    let array = row->Js.String2.split(" ")
    let opponents = array[0]->parseOpponentResponse
    (opponents, parseOwnResponseByOpponentResponse(opponents, array[1]))
  })
  ->calculate
}
