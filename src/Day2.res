exception InvalidInput

type response = Rock | Scissors | Paper

let parseMyResponse = input => {
  switch input {
  | "X" => Rock
  | "Y" => Paper
  | "Z" => Scissors
  | _ => raise(InvalidInput)
  }
}

let parseOpponentResponse = input => {
  switch input {
  | "A" => Rock
  | "B" => Paper
  | "C" => Scissors
  | _ => raise(InvalidInput)
  }
}

let parseResponseRow = input => {
  let array = input->Js.String2.split(" ")
  (array[0]->parseOpponentResponse, array[1]->parseMyResponse)
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

let parseInput = () => {
  Core.readInput()->Js.String2.split("\n")->Js.Array2.map(parseResponseRow)
}

let part1 = () => {
  parseInput()->Js.Array2.reduce(
    (result, (opponents, own)) =>
      result + calculateResultScore((opponents, own)) + calculateInputScore(own),
    0,
  )
}
