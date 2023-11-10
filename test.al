"1" parseInt
"1 a 3 b" " " split # => ["1", "a", "3", "b"]
"1 a 3 b" " " split (to_int) map # => [Ok(1), Err(), Ok(3), Err()]
"1 a 3 b" " " split (to_int) map flat # => [1, 3]
"1 a 3 b" " " split (to_int 0 orelse) map # => [1, 0, 3, 0]
"1 a 3 b" " " split (to_int 0 orelse 1 +) map # => [2, 1, 4, 1]
"1 a 3 b" " " split (to_int (1 +) map (0 orelse) map) map # => [2, 0, 4, 0]

# =========
# functions
# =========

work = (str on def)::(
  str on split (v)::(v to_int def orelse) map
)

string:"1 a 3 b" on:" " def:0 work # => [1, 0, 3, 0]
