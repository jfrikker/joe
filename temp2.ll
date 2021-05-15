%cons = type {
  i32,
  %cons*
}

%foldl_1 = type {
  i32(i32, i32)*,
  void(%foldl_2*, %foldl_1*, i32)*
}

%foldl_2 = type {
  i32(i32, i32)*,
  i32,
  i32(%foldl_2*, %cons*)*
}

define private void @foldl_0(%foldl_1* %sret, i32(i32, i32)* %f) {
  %1 = getelementptr %foldl_1, %foldl_1* %sret, i32 0, i32 0
  store i32(i32, i32)* %f, i32(i32, i32)** %1
  %2 = getelementptr %foldl_1, %foldl_1* %sret, i32 0, i32 1
  store void(%foldl_2*, %foldl_1*, i32)* @foldl_1, void(%foldl_2*, %foldl_1*, i32)** %2
  ret void
}

define private void @foldl_1(%foldl_2* %sret, %foldl_1* %closure, i32 %b) {
  %1 = getelementptr %foldl_1, %foldl_1* %closure, i32 0, i32 0
  %2 = getelementptr %foldl_2, %foldl_2* %sret, i32 0, i32 0
  %3 = load i32(i32, i32)*, i32(i32, i32)** %1
  store i32(i32, i32)* %3, i32(i32, i32)** %2
  %4 = getelementptr %foldl_2, %foldl_2* %sret, i32 0, i32 1
  store i32 %b, i32* %4
  %5 = getelementptr %foldl_2, %foldl_2* %sret, i32 0, i32 2
  store i32(%foldl_2*, %cons*)* @foldl_2, i32(%foldl_2*, %cons*)** %5
  ret void
}

define private i32 @foldl_2(%foldl_2* readonly %closure, %cons* %in) {
  %1 = getelementptr %foldl_2, %foldl_2* %closure, i32 0, i32 0
  %2 = load i32(i32, i32)*, i32(i32, i32)** %1
  %3 = getelementptr %foldl_2, %foldl_2* %closure, i32 0, i32 1
  %4 = load i32, i32* %3
  %5 = call i32 @foldl(i32(i32, i32)* %2, i32 %4, %cons* %in)
  ret i32 %5
}

define private i32 @foldl(i32(i32, i32)* %f, i32 %b, %cons* %in) {
  %1 = inttoptr i32 0 to %cons*
  %2 = icmp eq %cons* %in, %1
  br i1 %2, label %base, label %recur
  base:
  ret i32 %b
  recur:
  %3 = getelementptr %cons, %cons* %in, i32 0, i32 1
  %4 = load %cons*, %cons** %3
  %5 = call i32 @foldl(i32(i32, i32)* %f, i32 %b, %cons* %4)
  %6 = getelementptr %cons, %cons* %in, i32 0, i32 0
  %7 = load i32, i32* %6
  %8 = call i32 %f(i32 %7, i32 %5)
  ret i32 %8
}

define private i32 @add(i32 %a, i32 %b) {
  %1 = add i32 %a, %b
  ret i32 %1
}

define i32 @count(%cons* %in) {
  %1 = alloca %foldl_1
  %2 = alloca %foldl_2
  call void @foldl_0(%foldl_1* %1, i32(i32, i32)* @add)
  %3 = getelementptr %foldl_1, %foldl_1* %1, i32 0, i32 1
  %4 = load void(%foldl_2*, %foldl_1*, i32)*, void(%foldl_2*, %foldl_1*, i32)** %3
  call void %4(%foldl_2* %2, %foldl_1* %1, i32 0)
  %5 = getelementptr %foldl_2, %foldl_2* %2, i32 0, i32 2
  %6 = load i32(%foldl_2*, %cons*)*, i32(%foldl_2*, %cons*)** %5
  %7 = call i32 %6(%foldl_2* %2, %cons* %in)
  ret i32 %7
}