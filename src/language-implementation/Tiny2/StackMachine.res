/* homework 1 */
/* Write a interpreter for stack machine with variables */

type instr = Cst(int) | Add | Mul | Var(int) | Swap | Pop
type instrs = list<instr>
type stack = list<int>

let rec eval = (instrs: instrs, stack: stack) : int => {
  switch (instrs, stack) {
    | (list{Cst(i), ...rest}, stk) => eval(rest, list{i, ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
    | (list{Var(n), ...rest}, stk) => eval(rest, list{List.nth(stk, n), ...stk})
    | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
    | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
    | (list{}, list{result, ..._}) => result
  }
}

Js.log(
  eval(
    list{Cst(1), Cst(17), Var(0), Var(1), Add, Swap, Pop, Add},
    list{}
  )
) // 35
