type rec expr =
  | Cst(int) // int
  | Add(expr, expr) // a + b
  | Mul(expr, expr) // a * b

// recursive evaluator
let rec evalRec = expr => {
  switch expr {
  | Cst(i) => i
  | Add(e1, e2) => evalRec(e1) + evalRec(e2)
  | Mul(e1, e2) => evalRec(e1) * evalRec(e2)
  }
}

/**
 * homework 0
 * Implement the compilation algorithm in ReScript
 * Lowering expr to stack machine instrs(linearization)
*/
type instr = Cst(int) | Add | Mul // no recursive
type instrs = list <instr>
let concatMany = Belt.List.concatMany;

let rec compile = (expr: expr) : instrs => {
  switch expr {
    | Cst(i) => list{ Cst(i) }
    | Add(e1, e2) => concatMany([
        compile(e1),
        compile(e2),
        list{Add}
      ])
    | Mul(e1, e2) => concatMany([
        compile(e1),
        compile(e2),
        list{Mul}
      ])
  }
}

// eval stack machine instrs
type operand = int
type stack = list <operand>

let rec eval = (instrs: instrs, stk: stack) => {
  switch (instrs, stk) {
    | (list{}, list{result, ..._}) =>
      result
    | (list{Cst(i), ...rest}, _) =>
      eval(rest, list{i, ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) =>
      eval(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) =>
      eval(rest, list{a * b, ...stk})
    | _ => assert false
  }
}

Js.log(evalRec(
  Mul(
    Add(Cst(1), Cst(2)),
    Mul(Cst(2), Cst(2))
  )
)); // 12

Js.log(eval(
  compile(Add(Add(Cst(1), Cst(2)), Cst(3))),
  list{}
)); // 6
