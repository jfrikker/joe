%test_1 = type {
  i32,
  i32(%test_1*, i32)*
}

define private void @test_2(%test_1* %sret, i32 %a) {
  %1 = getelementptr %test_1, %test_1* %sret, i32 0, i32 0
  store i32 %a, i32* %1
  %2 = getelementptr %test_1, %test_1* %sret, i32 0, i32 1
  store i32(%test_1*, i32)* @test_1, i32(%test_1*, i32)** %2
  ret void
}

define private i32 @test_1(%test_1* %cl, i32 %b) {
  %1 = getelementptr %test_1, %test_1* %cl, i32 0, i32 0
  %2 = load i32, i32* %1
  %3 = add i32 %2, %b
  ret i32 %3
}

define i32 @main() {
  %1 = alloca %test_1
  call void @test_2(%test_1* %1, i32 1)
  %2 = getelementptr %test_1, %test_1* %1, i32 0, i32 1
  %3 = load i32(%test_1*, i32)*, i32(%test_1*, i32)** %2
  %4 = call i32 %3(%test_1* %1, i32 2)
  ret i32 %4
}