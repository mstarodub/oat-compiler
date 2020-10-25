define i64 @main(i64 %argc, i8** %arcv) {
    %1 = call i64 @lol1(i64 9, i64 8, i64 7, i64 6)
    ret i64 %1
}

define i64 @div(i64 %num, i64 %div) {
    %1 = icmp slt i64 %num, %div
    br i1 %1, label %rett, label %recc
rett:
    ret i64 0
recc:
    %2 = sub i64 %num, %div
    %3 = call i64 @div(i64 %2, i64 %div)
    %4 = add i64 %3, 1
    ret i64 %4
}

define i64 @lol4(i64 %0) {
  %2 = alloca i64
  store i64 %0, i64* %2
  %3 = load i64, i64* %2
  %4 = call i64 @div(i64 %3, i64 5)
  ret i64 %4
}

define i64 @lol3(i64 %0, i64 %1) {
  %3 = alloca i64
  %4 = alloca i64
  store i64 %0, i64* %3
  store i64 %1, i64* %4
  %5 = load i64, i64* %3
  %6 = load i64, i64* %4
  %7 = call i64 @lol4(i64 %6)
  %8 = mul i64 %5, %7
  ret i64 %8
}

define i64 @lol2(i64 %0, i64 %1, i64 %2) {
  %4 = alloca i64
  %5 = alloca i64
  %6 = alloca i64
  store i64 %0, i64* %4
  store i64 %1, i64* %5
  store i64 %2, i64* %6
  %7 = load i64, i64* %4
  %8 = load i64, i64* %5
  %9 = load i64, i64* %6
  %10 = call i64 @lol3(i64 %8, i64 %9)
  %11 = sub i64 %7, %10
  ret i64 %11
}

define i64 @lol1(i64 %0, i64 %1, i64 %2, i64 %3) {
  %5 = alloca i64
  %6 = alloca i64
  %7 = alloca i64
  %8 = alloca i64
  store i64 %0, i64* %5
  store i64 %1, i64* %6
  store i64 %2, i64* %7
  store i64 %3, i64* %8
  %9 = load i64, i64* %5
  %10 = load i64, i64* %6
  %11 = load i64, i64* %7
  %12 = load i64, i64* %8
  %13 = call i64 @lol2(i64 %10, i64 %11, i64 %12)
  %14 = add i64 %9, %13
  ret i64 %14
}
