# Alambre

This is my simple stack-oriented concatenative language.

## Why?

Why another language, aren't there enough already?

- Why not?
- It is fun?
- I could never pull myself together to learn `awk`.

This language is intended to be used as "wire" to attach command line tools 
together. It is not intended to be a general purpose language and it has some 
nice built in features that make stitching core utils together.

## How does it look?

```alm
// This solves advent of code day 1 part 1.
//
// Input via cli arg:
// '1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet'

// Alm program:

"\n" split 
(chars (to_int) map (is_some) filter first unwrap 10 * swap last unwrap swap drop +)
map 0 (+) reduce status
```

