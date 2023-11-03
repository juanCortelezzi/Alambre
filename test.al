# input "hello world!"
print a:input
# prints "hello world!" to stdout

# input "5"
parse_int base:10 str:input
# output 5

# input "hello\nthere"
split on:"\n" str:input
# output [ "hello", "there" ]

# input "hello\n\nthere"
split on:"\n\n" str:input
# output [ "hello", "there" ]

# input "hello, there"
split on:"," str:input
# output [ "hello", " there" ]

# input "hello there"
split on:" " str:input
# output [ "hello", "there" ]

# input "1 2 3"
map f:(parse_int base:10) iter:(split on:" " str:(trim str:input))
# output [ 1, 2, 3 ]

map 
  f:(parse_int base:10) 
  iter:(
    split on:" " str:(trim str:input)
  )

input
  |> trim str:_ 
  |> split on:"" str:_ 
  |> map f:(parse_int base:10 str:_) iter:_

  str:input trim on:"" split (str:_ base:10 parse_int) map

# input "\t 1\t  "
trim str:input
# output "1"

# input "\t 1\t  "
trim_left str:input
# output "1\t  "

# input "\t 1\t  "
trim_right str:input
# output "\t 1"
