open TemplateSyntax

let template_parse (t : string) : exp = 
  Template_Parser.program Template_Lexer.token (Lexing.from_string t)

let type_check_file (filename) =
    let cin = open_in filename in
    let template_exp = Template_Parser.program Template_Lexer.token (Lexing.from_channel cin)
    in TemplateTypeChecker.type_check template_exp TemplateSyntax.TPol


let p1 = "(fun (x : pol) : pol -> x) (true)"

TEST "Typeis test1" =  (eval (template_parse p1)) = Types.Filter Types.True


TEST "repeater.kat" = type_check_file ("examples/templates/repeater.kat")

TEST "Chapter7.kat" = type_check_file ("examples/templates/Chapter7.kat")

TEST "Chapter8.kat" = type_check_file ("examples/templates/Chapter8.kat")

TEST "repeater_link.kat" = type_check_file ("examples/templates/repeater_link.kat")

