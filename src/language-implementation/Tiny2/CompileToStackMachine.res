/* homework 1 */
/* Write a compiler to translate Nameless.expr to stack machine instrs */

type sv = Slocal | Stmp
type stackEnv = list<sv>
type instr = Cst(int) | Add | Mul | Var(int) | Swap | Pop; // without Let
// type instrs = list<instr>

let concatMany = Belt.List.concatMany;
let sindex = (cenv: stackEnv, n: int) => {
  let rec go = (cenv, n, index) => {
    switch cenv {
      | list{} => raise (Not_found)
      | list{Stmp, ...rest} => go(rest, n, index + 1)
      | list{Slocal, ...rest} => if n === 0 { index } else { go(rest, n - 1, index + 1) }
    }
  }
  go(cenv, n, 0)
}

let rec compileToStackMachine = (expr: Nameless.expr, cenv: stackEnv) : EvalInstrs.instrs => {
  switch expr {
    | Cst(i) => list{ Cst(i) }
    | Add(e1, e2) => concatMany([
      compileToStackMachine(e1, cenv),
      compileToStackMachine(e2, list{Stmp, ...cenv}),
      list{ Add }
    ])
    | Mul(e1, e2) => concatMany([
      compileToStackMachine(e1, cenv),
      compileToStackMachine(e2, list{Stmp, ...cenv}),
      list{ Mul }
    ])
    | Var(n) => list{Var(sindex(cenv, n))}
    | Let(e1, e2) => concatMany([
      compileToStackMachine(e1, cenv),
      compileToStackMachine(e2, list{Slocal, ...cenv}),
      list{Swap, Pop}
    ])
  }
}

Js.log(
  EvalInstrs.evalInstrs(
    compileToStackMachine(
      Add(Cst(1), Let(Cst(3), Add(Var(0), Var(0)))),
      list{}
    ),
    list{}
  )
) // 5
