class Day15Tests extends munit.FunSuite:
  import day15.*
  import Hash.*

  test("hash"):
    assert(Hash("HASH") == 52)

  test("test full part 1"):
    val input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    assert(part1(input) == 1320)
