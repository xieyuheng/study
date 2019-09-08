package xieyuheng.cicada.with_logic_variable

object lexerTest extends App {
  println(parser.lexer.lex("1 2 3"))
  println(parser.lexer.lex("1 2 3 // a b c"))
  println(parser.lexer.lex("1(2)3"))
  println(parser.lexer.lex("succ_t(nat_add(x.prev, y))"))
}
