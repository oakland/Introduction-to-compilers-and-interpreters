type rec expr =
  | Cst(int) // int
  | Add(expr, expr) // a + b
  | Mul(expr, expr) // a * b
  | Var(string)
  | Let(string, expr, expr)


// recursive evaluator
let rec evalRec = (expr, env) => {
  switch expr {
    | Cst(i) => i
    | Add(e1, e2) => evalRec(e1, env) + evalRec(e2, env)
    | Mul(e1, e2) => evalRec(e1, env) * evalRec(e2, env)
    | Var(x) => List.assoc(x, env)
    | Let(x, e1, e2) => evalRec(e2, list{(x, evalRec(e1, env)), ...env})
  }
}

module Nameless = {
  type rec expr = 
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  type stack = list<int>
  let rec eval = (expr: expr, env: stack) : int => {
    switch expr {
      | Cst(i) => i
      | Add(a, b) => eval(a, env) + eval(b, env)
      | Mul(a, b) => eval(a, env) * eval(b, env)
      | Var(int) => List.nth(env, int)
      | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    }
  } 
}

let index = (cenv, x) : int => {
  let rec go = (cenv, n) => {
    switch cenv {
      | list{} => raise (Not_found)
      | list{a, ...rest} => if a === x {n} else { go(rest, n+1) }
    }
  }
  go(cenv, 0);
}

type cenv = list<string>

// compile to Nameless expresion
let rec compileToNameless = (expr: expr, cenv) : Nameless.expr => {
  switch expr {
    | Cst(i) => Cst(i)
    | Add(e1, e2) => Add(compileToNameless(e1, cenv), compileToNameless(e2, cenv))
    | Mul(e1, e2) => Mul(compileToNameless(e1, cenv), compileToNameless(e2, cenv))
    | Var(x) => Var(index(cenv, x))
    | Let(x, e1, e2) => Let(compileToNameless(e1, cenv), compileToNameless(e2, list{x, ...cenv}))
  }
}

/*
module StackMachine = {
  type expr = Cst(int) | Add | Mul | Var(int) | Let;
}

type instrs = list<StackMachine.expr>;

// compile to stack machine
let rec compileToStackMachine = (expr: Nameless.expr, cenv) : instrs => {
  //
}
*/

Js.log(Nameless.eval(compileToNameless(
  Let("dozon", Add(Cst(6), Cst(6)), Mul(Var("dozon"), Var("dozon"))),
  list{}
), list{}));
