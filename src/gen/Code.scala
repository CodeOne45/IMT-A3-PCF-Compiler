package gen

enum Code {
  case Ins(ins: String)
  case Seq(seq: List[Code])
  case Test(code1: Code, code2: Code)
}