type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(int)
  | Let(expr, expr)
  | Fn(expr)
  | App(expr, list<expr>)

type rec value =
  | Vint(int)
  | VClosure(env, expr)
and env = list<value>

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
    | Var(n) => List.nth(env, n)
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    | Fn(e) => VClosure(env, e)
    | App(e, es) => {
      let VClosure(env_closure, body) = eval(e, env)
      let vs = map(es, e => eval(e, env))
      let fun_env = concatMany([ vs, env_closure ])
      eval(body, fun_env)
    }
  }
}

Js.log(eval(
  App(Fn(Add(Var(0), Var(1))), list{Cst(1), Cst(2)}),
  list{}
)); // Vint(3)
