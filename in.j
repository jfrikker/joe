test :: I32
test = 123i32

test2 :: I64
test2 = 456i64

test3 :: I32
test3 = 123i32 + 456i32

test4 :: I32 -> I32 -> I32 -> I32
test4 a b c = a + b + c

test5 :: I32 -> I32
test5 = test4 5i32 10i32

main :: I32
main = 3i32 + 5i32
