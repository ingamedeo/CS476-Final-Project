; ModuleID = 'function.ll'
source_filename = "./functions/function.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.str = private unnamed_addr constant [13 x i8] c"a is now %f\0A\00", align 1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local void @change_value_to(float*) #0 {
  %2 = alloca float*, align 8
  store float* %0, float** %2, align 8
  %3 = call i32 @rand() #3
  %4 = sitofp i32 %3 to float
  %5 = load float*, float** %2, align 8
  store float %4, float* %5, align 4
  ret void
}

; Function Attrs: nounwind
declare i32 @rand() #1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local float* @change_and_return(float*) #0 {
  %2 = alloca float*, align 8
  store float* %0, float** %2, align 8
  %3 = call i32 @rand() #3
  %4 = sitofp i32 %3 to float
  %5 = load float*, float** %2, align 8
  store float %4, float* %5, align 4
  %6 = load float*, float** %2, align 8
  ret float* %6
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @main() #0 {
  %1 = alloca float, align 4
  store float 3.000000e+00, float* %1, align 4
  call void @change_value_to(float* %1)
  %2 = load float, float* %1, align 4
  %3 = fpext float %2 to double
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str, i64 0, i64 0), double %3)
  ret i32 0
}

declare i32 @printf(i8*, ...) #2

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 9.0.0 (tags/RELEASE_900/final)"}
