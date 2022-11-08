type rec expr =
  | Cst(int) // int
  | Add(expr, expr) // a + b
  | Mul(expr, expr) // a * b

type instr = Cst(int) | Add | Mul // no recursive
type instrs = list <instr>
type operand = int
type stack = list <operand>

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
*/
// TODO: using spread operator to append list
let append = List.append;
let rec compile = (expr: expr) : instrs => {
  switch expr {
    | Cst(i) => list{ Cst(i) }
    | Add(e1, e2) => append(
        append(compile(e1), compile(e2)),
        list{Add}
      )
    | Mul(e1, e2) => append(
        append(compile(e1), compile(e2)),
        list{Mul}
      )
  }
}

// linear evaluator
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

Js.log(evalRec(Cst(1))); // 1
Js.log(evalRec(
  Add(Cst(1), Cst(2))
)); // 3
Js.log(evalRec(
  Add(Add(Cst(1), Cst(2)), Cst(3))
)); // 6
Js.log(evalRec(
  Mul(Cst(1), Cst(2))
)); // 2
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
