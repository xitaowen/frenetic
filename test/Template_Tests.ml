open TemplateSyntax

let template_parse (t : string) : exp = 
  Template_Parser.program Template_Lexer.token (Lexing.from_string t)



let p1 = "(fun x : pol : pol = x) (filter true)"
TEST "Typeis test1" =  (eval (template_parse p1)) = NetKAT_Types.Filter (NetKAT_Types.True) 
