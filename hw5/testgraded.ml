open Assert
open Ast

(* TODO: change these into actual, more complicated tests, e.g. permute the fields *)

let unit_ok = Test ("gradedtest unit",
  [ "typ_structex", (fun () ->
    let c = Tctxt.add_struct Tctxt.empty "a" [{fieldName="x";ftyp=TInt}] in
    let c = Tctxt.add_struct c "b" [{fieldName="x";ftyp=TInt}] in
    if Typechecker.subtype c (TRef (RStruct "a")) (TRef (RStruct "b"))
    then ()
    else failwith "should not fail") ]
  )

let unit_fail = Test ("gradedtest unit fail",
  [ "typ_structex fail", (fun () ->
    let c = Tctxt.add_struct Tctxt.empty "a" [{fieldName="x";ftyp=TInt}] in
    let c = Tctxt.add_struct c "b" [{fieldName="y";ftyp=TInt}] in
    if Typechecker.subtype c (TRef (RStruct "a")) (TRef (RStruct "b"))
    then failwith "should not succeed"
    else ()) ]
  )

let graded_unit_tests : suite =
  [ unit_ok
  ; unit_fail ]
