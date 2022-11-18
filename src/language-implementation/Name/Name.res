// Name expression
type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(string) // *new feature
  | Let(string, expr, expr) // *new feature

type env = list<(string, int)>

let rec evalRec = (expr: expr, env: env) => {
  switch expr {
    | Cst(i) => i
    | Add(e1, e2) => evalRec(e1, env) + evalRec(e2, env)
    | Mul(e1, e2) => evalRec(e1, env) * evalRec(e2, env)
    | Var(x) => List.assoc(x, env)
    | Let(x, e1, e2) => evalRec(e2, list{(x, evalRec(e1, env)), ...env})
  }
}

Js.log(evalRec(
  Add(Let("dozon", Cst(12), Add(Var("dozon"), Var("dozon"))), Cst(2)), list{}
)); // 26
