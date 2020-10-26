; ModuleID = 'gep9.ll'
source_filename = "gep9.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct = type { i64, [5 x i64], i64 }

@gbl = local_unnamed_addr global %struct { i64 1, [5 x i64] [i64 2, i64 3, i64 4, i64 5, i64 6], i64 7 }

; Function Attrs: norecurse nounwind readonly
define i64 @main(i64 %argc, i8** nocapture readnone %arcv) local_unnamed_addr #0 {
  %1 = load i64, i64* getelementptr inbounds (%struct, %struct* @gbl, i64 0, i32 1, i64 3), align 8
  ret i64 %1
}

attributes #0 = { norecurse nounwind readonly }
