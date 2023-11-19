"1" to_int # => Ok(1)
"1 a 3 b" " " split # => ["1", "a", "3", "b"]
"1 a 3 b" " " split (to_int) map # => [o1, e, o3, e]
"1 a 3 b" " " split (to_int) map flat # => [1, 3]
"1 a 3 b" " " split (to_int 0 orelse) map # => [1, 0, 3, 0]
"1 a 3 b" " " split (to_int 0 orelse 1 +) map # => [2, 1, 4, 1]
"1 a 3 b" " " split (to_int (1 +) map (0 orelse) map) map # => [2, 0, 4, 0]

"adsfads"
[data_type string "adsfads"]
> push it in the stack

split
[builtin al_split]
> execute the action

(to_int)
[builtin al_fn (builtin al_to_int)]
> push it in the stack

1 1 = if "equal" else "not equal" end # => "equal"
[
  data_type int 1,
  data_type int 1,
  builtin equal,
  keyword if,
  data_type string "equal",
  keyword else,
  data_type string "not equal",
  keyword end
]
[
  data_type int 1,
  data_type int 1,
  builtin equal,
  statement if {
    true: data_type string "equal",
    false: data_type string "not equal",
  }
]




1 2 = if "equal" else "not equal" end # => "not equal"

1 1 = if "equal" else "not equal" end # => "equal"
1 2 = if "equal" else "not equal" end # => "not equal"

more_than_ten::(10 > not if 1 + more_than_ten end) 4 more_than_ten

[1,2,3,4,5,6] (dup 3 > if to_some else none end) map # => [n, n, n, s4, s5, s6]
