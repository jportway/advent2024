object Day17 {

  case class State(memory: IndexedSeq[Int], pc: Int, a: Long, b: Long, c: Long)

  def readCombo(operand: Int, state: State): Long = {
    operand match {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 3
      case 4 => state.a
      case 5 => state.b
      case 6 => state.c
    }
  }

  def execute(state: State): (State, Option[Int]) = {
    val opcode  = state.memory(state.pc)
    val operand = state.memory(state.pc + 1)
    val (newState, output) = opcode match {
      case 0 => adv(state, operand)
      case 1 => bxl(state, operand)
      case 2 => bst(state, operand)
      case 3 => jnz(state, operand)
      case 4 => bxc(state, operand)
      case 5 => out(state, operand)
      case 6 => bdv(state, operand)
      case 7 => cdv(state, operand)
    }
    (newState, output)
  }

  def run(initialState: State) = LazyList
    .unfold(initialState) { state =>
      if state.pc >= state.memory.length then None
      else {
        val (newState, output) = execute(state)
        Some(output, newState)
      }
    }
    .collect { case Some(output) => output }

  def adv(state: State, operand: Int): (State, Option[Int]) = {
    val result = state.a / (Math.pow(2, readCombo(operand, state)).toInt)
    (state.copy(pc = state.pc + 2, a = result), None)
  }

  def bxl(state: State, operand: Int): (State, Option[Int]) =
    (state.copy(pc = state.pc + 2, b = state.b ^ operand), None)

  def bst(state: State, operand: Int): (State, Option[Int]) =
    (state.copy(pc = state.pc + 2, b = readCombo(operand, state) % 8), None)

  def jnz(state: State, operand: Int): (State, Option[Int]) =
    if state.a == 0 then (state.copy(pc = state.pc + 2), None)
    else (state.copy(pc = operand), None)

  def bxc(state: State, operand: Int): (State, Option[Int]) =
    (state.copy(pc = state.pc + 2, b = state.b ^ state.c), None)

  def out(state: State, operand: Int): (State, Option[Int]) = {
    val r = readCombo(operand, state) % 8
    (state.copy(pc = state.pc + 2), Some(r.toInt))
  }

  def bdv(state: State, operand: Int): (State, Option[Int]) = {
    val result = state.a / (Math.pow(2, readCombo(operand, state)).toInt)
    (state.copy(pc = state.pc + 2, b = result), None)
  }

  def cdv(state: State, operand: Int): (State, Option[Int]) = {
    val result = state.a / (Math.pow(2, readCombo(operand, state)).toInt)
    (state.copy(pc = state.pc + 2, c = result), None)
  }

  @main
  def day17Main(): Unit = {
    val testState = State(Vector(0, 1, 5, 4, 3, 0), 0, 729, 0, 0)
    val result    = run(testState).toList
    println(result)

    val aState  = State(Vector(2, 4, 1, 3, 7, 5, 4, 7, 0, 3, 1, 5, 5, 5, 3, 0), 0, 52884621, 0, 0)
    val resultA = run(aState).toList.mkString(",")
    println(s"part A : $resultA")

    // this is the program
    // BST(A), BXL(3), CDV(B), BXC(7), ADV(3), BXL(5), OUT(B), JNZ(0)
    // B = A % 8 -- first 3 bits of a
    // B = B ^ 3 -- flip lower 2 bits
    // C = A / B
    // B = B ^ C --
    // A = A / 8 -- shift a right 3 bits
    // B = B ^ 5 -- b=7
    // OUT(B)
    // JUMP

    // we are going to find a value that produces each digit one at a time, starting from the last digit
    // and then using the result of that (shifted left 3 bits) as the basis of the search for the next digit etc.

    val memory = Vector(2, 4, 1, 3, 7, 5, 4, 7, 0, 3, 1, 5, 5, 5, 3, 0)
    val (_, code) = memory.reverse.foldLeft((List.empty[Int], 0L)) { case ((targetAcc, digitAcc), digit) =>
      val newTarget = digit :: targetAcc
      val newDigit  = firstMatch(newTarget, digitAcc * 8)
      (newTarget, newDigit)
    }
    val verifyState = State(memory, 0, code, 0, 0)
    assert(run(verifyState).toList == memory) // verify it with the original program
    println(s"part B:$code")
  }

  def firstMatch(target: List[Int], start: Long) = {
    var i = start - 1
    while {
      i = i + 1
      calcDigits(i) != target
    } do ()
    i
  }

  // faster version of code
  def calcDigits(startA: Long): List[Int] = {
    List.unfold(startA) { a =>
      if a == 0 then None
      else {
        val b      = (a  % 8) ^ 3
        val c      = a / (Math.pow(2, b).toLong) // b has max of 7
        val b2     = (b ^ c) ^ 5
        val result = (b2 % 8).toInt
        Some(result, a / 8)
      }
    }
  }

}
