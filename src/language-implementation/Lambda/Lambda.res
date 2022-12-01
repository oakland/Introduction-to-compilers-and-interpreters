type rec lambda =
  | Var(string)
  | Fn(string, lambda) // accept only one parameter
  | App(lambda, lambda)

// substitution without free variables
let rec subst = (x : string, v: lambda, body: lambda) : lambda => {
  switch body {
    | Var(y) => if x == y { v } else { body }
    | Fn(y, t) => if x == y { body } else { Fn(y, subst(x, v, t)) }
    | App(a, b) => App(subst(x, v, a), subst(x, v, b))
  }
}

let rec eval = (t: lambda) : lambda => {
  switch t {
    | Var(_) => assert false
    | Fn(_, _) => t
    | App(f, arg) => {
      let Fn(x, body) = eval(f)
      let v = eval(arg)
      eval(subst(x, v, body))
    }
  }
}

Js.log(
  eval(
    App(Fn("sth", Var("sth")), Fn("num", Var("num")))
  )
)
