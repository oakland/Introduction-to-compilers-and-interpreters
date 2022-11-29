/* homework 1 */
/* Write a compiler to translate Nameless.expr to stack machine instrs */

type sv = Slocal | Stmp
type stackEnv = list<sv>
// type instr = Cst(int) | Add | Mul | Var(int) | Swap | Pop; // without Let
// type instrs = list<instr>

let concatMany = Belt.List.concatMany;
let sindex = (cenv: stackEnv, n: int) => {
  let rec go = (cenv, n, cur) => {
    switch cenv {
      | list{} => raise (Not_found)
      | list{Stmp, ...rest} => go(rest, n, cur + 1)
      | list{Slocal, ...rest} => if n === 0 { cur } else { go(rest, n - 1, cur + 1) }
    }
  }
  go(cenv, n, 0)
}

// Nameless.expr to StackMachine.instrs
let rec compile = (expr: Nameless.expr, cenv: stackEnv) : StackMachine.instrs => {
  switch expr {
    | Cst(i) => list{ Cst(i) }
    | Add(e1, e2) => concatMany([
      compile(e1, cenv),
      compile(e2, list{Stmp, ...cenv}),
      list{ Add }
    ])
    | Mul(e1, e2) => concatMany([
      compile(e1, cenv),
      compile(e2, list{Stmp, ...cenv}),
      list{ Mul }
    ])
    | Var(n) => list{Var(sindex(cenv, n))}
    | Let(e1, e2) => concatMany([
      compile(e1, cenv),
      compile(e2, list{Slocal, ...cenv}),
      list{Swap, Pop}
    ])
  }
}

Js.log(
  StackMachine.eval(
    compile(
      Add(Cst(1), Let(Cst(3), Add(Var(0), Var(0)))),
      list{}
    ),
    list{}
  )
) // 7
