type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(string)
  | Let(string, expr, expr)
  | Fn(list<string>, expr) // *new feature
  | App(expr, list<expr>) // *new feature

type rec value =
  | Vint(int)
  | VClosure(env, list<string>, expr)
and env = list<(string, value)>

let vadd = (v1, v2) : value => {
  switch(v1, v2) {
    | (Vint(i), Vint(j)) => Vint(i + j)
    | _ => assert false
  }
}

let vmul = (v1, v2) : value => {
  switch(v1, v2) {
    | (Vint(i), Vint(j)) => Vint(i * j)
    | _ => assert false
  }
}

let map = Belt.List.map;
let concatMany = Belt.List.concatMany;
let zip = Belt.List.zip;
let rec eval = (expr: expr, env: env) : value => {
  switch expr {
    | Cst(i) => Vint(i)
    | Add(e1, e2) => vadd(eval(e1, env), eval(e2, env))
    | Mul(e1, e2) => vmul(eval(e1, env), eval(e2, env))
    | Var(x) => List.assoc(x, env)
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
    | Fn(xs, e) => VClosure(env, xs, e)
    | App(e, es) => {
      let VClosure(env_closure, xs, body) = eval(e, env)
      let vs = map(es, v => eval(v, env))
      let fun_env = concatMany([ zip(xs, vs), env_closure ])
      eval(body, fun_env)
    }
  }
}

Js.log(eval(
  App(Fn(list{"one", "two"}, Add(Var("one"), Var("two"))), list{Cst(1), Cst(2)}),
  list{}
)); // Vint(3)
