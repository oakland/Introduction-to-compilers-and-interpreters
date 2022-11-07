// Tiny0
type rec expr =
  | Cst(int) // int
  | Add(expr, expr) // a + b
  | Mul(expr, expr) // a * b

// Evaluator
let rec eval = expr => {
  switch expr {
  | Cst(i) => i
  | Add(e1, e2) => eval(e1) + eval(e2)
  | Mul(e1, e2) => eval(e1) * eval(e2)
  }
}

Js.log(eval(Cst(1))); // 1
Js.log(eval(
  Add(Cst(1), Cst(2))
)); // 3
Js.log(eval(
  Add(Add(Cst(1), Cst(2)), Cst(3))
)); // 6
Js.log(eval(
  Mul(Cst(1), Cst(2))
)); // 2
Js.log(eval(
  Mul(
    Add(Cst(1), Cst(2)),
    Mul(Cst(2), Cst(2))
  )
)); // 12
