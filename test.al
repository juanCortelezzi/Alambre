"1" parseInt
"1 a 3 b" " " split # => ["1", "a", "3", "b"]
"1 a 3 b" " " split (to_int) map # => [Ok(1), Err(), Ok(3), Err()]
"1 a 3 b" " " split (to_int) map flat # => [1, 3]
"1 a 3 b" " " split (to_int 0 ordefault) map # => [1, 0, 3, 0]
"1 a 3 b" " " split (to_int 0 ordefault (1 +) map) map # => [2, 0, 4, 0]

# =========
# functions
# =========

work = (string on def)::(
  string on split (parseInt def ordefault) map
)

string:"1 a 3 b" on:" " def:0 work # => [1, 0, 3, 0]
