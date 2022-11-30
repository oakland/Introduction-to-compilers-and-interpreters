type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(int)
  | Let(expr, expr)

type env = list<int>

let rec eval = (expr: expr, env: env) => {
  switch expr {
    | Cst(i) => i
    | Add(e1, e2) => eval(e1, env) + eval(e2, env)
    | Mul(e1, e2) => eval(e1, env) * eval(e2, env)
    | Var(n) => List.nth(env, n)
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
  }
}

Js.log(eval(
  Add(Let(Cst(12), Add(Var(0), Var(0))), Cst(2)),
  list{}
)); // 26
