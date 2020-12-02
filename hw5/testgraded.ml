open Assert
open Ast

let unit_ok = Test ("gradedtest unit",
  [ "sub_substruct", (fun () ->
    let c = Tctxt.add_struct Tctxt.empty "a" [
        {fieldName="x";ftyp=
          TRef (RFun ([TNullRef RString], RetVal (TNullRef (RArray TInt))))
        }
      ; {fieldName="y";ftyp=TNullRef (RStruct "a")}
      ; {fieldName="z";ftyp=TBool} ] in
    let c = Tctxt.add_struct c "b" [
        {fieldName="x";ftyp=
          TRef (RFun ([TNullRef RString], RetVal (TNullRef (RArray TInt))))
        }
      ; {fieldName="y";ftyp=TNullRef (RStruct "a")} ] in
    if Typechecker.subtype c (TRef (RStruct "a")) (TRef (RStruct "b"))
    then ()
    else failwith "should not fail") ]
  )

let unit_fail = Test ("gradedtest unit fail",
  [ "no_sub_substruct", (fun () ->
    let c = Tctxt.add_struct Tctxt.empty "a" [
        {fieldName="x";ftyp=TInt}
      ; {fieldName="y";ftyp=TRef (RArray TBool)}
      ; {fieldName="z";ftyp=TNullRef RString} ] in
    let c = Tctxt.add_struct c "b" [
        {fieldName="w";ftyp=TInt}
      ; {fieldName="z";ftyp=TRef (RArray TBool)}
      ; {fieldName="x";ftyp=TNullRef RString}
      ; {fieldName="y";ftyp=TRef (RArray (TRef RString))} ] in
    if Typechecker.subtype c (TRef (RStruct "a")) (TRef (RStruct "b"))
    then failwith "should not succeed"
    else ()) ]
  )

(* the two unit tests *)
let graded_unit_tests : suite =
  [ unit_ok
  ; unit_fail ]

let oatprogram_test : suite = [
  Test("minicompiler", Gradedtests.executed_oat_file ["minicompile.oat", "\"2 * ( 1 + 2) ;\"", "# 2 1 2 + * ; \n.text\n.globl main\nmain:\n\tmovq $2, %rax\n\tpushq %rax\n\tmovq $1, %rax\n\tpushq %rax\n\tmovq $2, %rax\n\tpushq %rax\n\tpopq %rdx\n\tpopq %rax\n\taddq %rdx, %rax\n\tpushq %rax\n\tpopq %rdx\n\tpopq %rax\n\timulq %rdx, %rax\n\tpushq %rax\n\tpopq %rax\n\tretq\n"])
]
