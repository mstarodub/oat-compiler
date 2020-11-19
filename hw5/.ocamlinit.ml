#use "topfind";;

#directory "_build"
#directory "_build/util"
#directory "_build/x86"
#directory "_build/grading"
#directory "_build/ll"

#mod_use "util/range.ml"
#mod_use "util/assert.ml"
#mod_use "util/platform.ml"

#load_rec "x86.cmo"
#load_rec "ll.cmo"

#mod_use "ll/ll.ml"
#mod_use "ll/llutil.ml"
#mod_use "ll/llparser.ml"
#mod_use "ll/lllexer.ml"

#mod_use "ast.ml"
#mod_use "astlib.ml"

#mod_use "backend.ml"

#mod_use "typechecker.ml"
#mod_use "frontend.ml"
#mod_use "tctxt.ml"
