/*
type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(int)
  | Let(expr, expr)


let rec evalRec = (expr, env) => {
  switch expr {
    | Cst(i) => i
    | Add(e1, e2) => evalRec(e1, env) + evalRec(e2, env)
    | Mul(e1, e2) => evalRec(e1, env) * evalRec(e2, env)
    | Var(n) => List.nth(env, n)
    | Let(e1, e2) => evalRec(e2, list{evalRec(e1, env), ...env})
  }
}

let append = List.append;
let rec compile = (expr, cenv) => {
  switch expr {
    | Cst(i) => i
    | Add(e1, e2) => append(append(compile(e1), compile(e2)), Add)
    | Mul(e1, e2) => append(append(compile(e1), compile(e2)), Mul)
    | Var(n) => List.nth(cenv, n)
    | Let(e1, e2) => List.nth(cenv, n)
  }
}

Js.log(evalRec(
  Let(Cst(12), Add(Var(0), Add(Cst(1), Var(0)))),
  list{}
)); // 25
*/
