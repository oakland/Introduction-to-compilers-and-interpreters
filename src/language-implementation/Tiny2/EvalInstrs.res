/* homework 1 */
/* Write a interpreter for stack machine with variables */

type instr = Cst(int) | Add | Mul | Var(int) | Swap | Pop
type instrs = list<instr>
type stack = list<int>

let rec evalInstrs = (instrs: instrs, stack: stack) : int => {
  switch (instrs, stack) {
    | (list{Cst(i), ...rest}, stk) => evalInstrs(rest, list{i, ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) => evalInstrs(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => evalInstrs(rest, list{a * b, ...stk})
    | (list{Var(n), ...rest}, stk) => evalInstrs(rest, list{List.nth(stk, n), ...stk})
    | (list{Swap, ...rest}, list{a, b, ...stk}) => evalInstrs(rest, list{b, a, ...stk})
    | (list{Pop, ...rest}, list{_, ...stk}) => evalInstrs(rest, stk)
    | (list{}, list{result, ..._}) => result
  }
}

Js.log(
  evalInstrs(
    list{Cst(1), Cst(17), Var(0), Var(1), Add, Swap, Pop, Add},
    list{}
  )
) // 35
