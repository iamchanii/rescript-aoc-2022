// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core from "./Core.bs.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var InvalidInput = /* @__PURE__ */Caml_exceptions.create("Day2.InvalidInput");

function parseMyResponse(input) {
  switch (input) {
    case "X" :
        return /* Rock */0;
    case "Y" :
        return /* Paper */2;
    case "Z" :
        return /* Scissors */1;
    default:
      throw {
            RE_EXN_ID: InvalidInput,
            Error: new Error()
          };
  }
}

function parseOpponentResponse(input) {
  switch (input) {
    case "A" :
        return /* Rock */0;
    case "B" :
        return /* Paper */2;
    case "C" :
        return /* Scissors */1;
    default:
      throw {
            RE_EXN_ID: InvalidInput,
            Error: new Error()
          };
  }
}

function parseResponseRow(input) {
  var array = input.split(" ");
  return [
          parseOpponentResponse(Caml_array.get(array, 0)),
          parseMyResponse(Caml_array.get(array, 1))
        ];
}

function calculateResultScore(round) {
  switch (round[0]) {
    case /* Rock */0 :
        switch (round[1]) {
          case /* Rock */0 :
              return 3;
          case /* Scissors */1 :
              return 0;
          case /* Paper */2 :
              return 6;
          
        }
    case /* Scissors */1 :
        switch (round[1]) {
          case /* Rock */0 :
              return 6;
          case /* Scissors */1 :
              return 3;
          case /* Paper */2 :
              return 0;
          
        }
    case /* Paper */2 :
        switch (round[1]) {
          case /* Rock */0 :
              return 0;
          case /* Scissors */1 :
              return 6;
          case /* Paper */2 :
              return 3;
          
        }
    
  }
}

function calculateInputScore(input) {
  switch (input) {
    case /* Rock */0 :
        return 1;
    case /* Scissors */1 :
        return 3;
    case /* Paper */2 :
        return 2;
    
  }
}

function parseInput(param) {
  return Core.readInput(undefined).split("\n").map(parseResponseRow);
}

function part1(param) {
  return parseInput(undefined).reduce((function (result, param) {
                var own = param[1];
                return (result + calculateResultScore([
                              param[0],
                              own
                            ]) | 0) + calculateInputScore(own) | 0;
              }), 0);
}

export {
  InvalidInput ,
  parseMyResponse ,
  parseOpponentResponse ,
  parseResponseRow ,
  calculateResultScore ,
  calculateInputScore ,
  parseInput ,
  part1 ,
}
/* Core Not a pure module */
