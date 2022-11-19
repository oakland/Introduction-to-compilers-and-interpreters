type compileEnv = list<string>

let index = (cenv: compileEnv, x: string) => {
  let rec go = (cenv: compileEnv, index: int) => {
    switch cenv {
      | list{} => raise (Not_found)
      | list{a, ...rest} =>
        if a === x { index } else { go(rest, index+1) }
    }
  }
  go(cenv, 0)
}

let rec compileToNameless = (expr: Name.expr, cenv: compileEnv) : Nameless.expr => {
  switch expr {
    | Cst(i) => Cst(i)
    | Add(e1, e2) => Add(compileToNameless(e1, cenv), compileToNameless(e2, cenv))
    | Mul(e1, e2) => Mul(compileToNameless(e1, cenv), compileToNameless(e2, cenv))
    | Var(x) => Var(index(cenv, x))
    | Let(x, e1, e2) => Let(compileToNameless(e1, cenv), compileToNameless(e2, list{x, ...cenv}))
  }
}

Js.log(
  Nameless.evalRec(
    compileToNameless(
      Add(Let("dozon", Cst(12), Add(Var("dozon"), Var("dozon"))), Cst(2)),
      list{}
    ),
    list{}
  )
)
