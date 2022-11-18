module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  type env = list<int>

  let rec evalRec = (expr: expr, env: env) => {
    switch expr {
      | Cst(i) => i
      | Add(e1, e2) => evalRec(e1, env) + evalRec(e2, env)
      | Mul(e1, e2) => evalRec(e1, env) * evalRec(e2, env)
      | Var(n) => List.nth(env, n)
      | Let(e1, e2) => evalRec(e2, list{evalRec(e1, env), ...env})
    }
  }
}

Js.log(Nameless.evalRec(
  Let(Cst(12), Add(Var(0), Add(Cst(1), Var(0)))),
  list{}
)); // 25
