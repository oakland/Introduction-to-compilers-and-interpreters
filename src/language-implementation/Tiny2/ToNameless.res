type compileEnv = list<string>

let index = (cenv: compileEnv, x: string) => {
  let rec go = (cenv: compileEnv, cur: int) => {
    switch cenv {
      | list{} => raise (Not_found)
      | list{a, ...rest} =>
        if a === x { cur } else { go(rest, cur+1) }
    }
  }
  go(cenv, 0)
}

// Compile from name expression to nameless expression
let rec compile = (expr: Name.expr, cenv: compileEnv) : Nameless.expr => {
  switch expr {
    | Cst(i) => Cst(i)
    | Add(e1, e2) => Add(compile(e1, cenv), compile(e2, cenv))
    | Mul(e1, e2) => Mul(compile(e1, cenv), compile(e2, cenv))
    | Var(x) => Var(index(cenv, x))
    | Let(x, e1, e2) => Let(compile(e1, cenv), compile(e2, list{x, ...cenv}))
  }
}

Js.log(
  Nameless.evalRec(
    compile(
      Add(Let("dozon", Cst(12), Add(Var("dozon"), Var("dozon"))), Cst(2)),
      list{}
    ),
    list{}
  )
)
