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

Js.log(evalRec(
  Let("dozon", Cst(12), Add(Var("dozon"), Add(Cst(1), Var("dozon")))),
  list{}
)); // 25
