
trait State;

case class IntState(i: Int) extends State {
  override def toString(): String = i.toString;
}
implicit val toIntState = (i: Int) => IntState(i);


case class FARule (state: State, ch: Char, nextState: State) {

  def appliesTo(state: State, ch: Char): Boolean = (state == this.state && ch == this.ch);

  def follow: State = nextState;

  override def toString(): String = "#<FARule " + state + " --" + ch + "--> " + nextState + ">";

}

case class DFARulebook (rules: Seq[FARule]) {

  def nextState(state: State, ch: Char): State = ruleFor(state, ch).follow;

  def ruleFor(state: State, ch: Char): FARule = rules.find(_.appliesTo(state, ch)).get;

}

class DFA (_currentState: State, acceptStates: Seq[State], rulebook: DFARulebook) {

  private[this] var currentState: State = _currentState;

  def accepting: Boolean = acceptStates.contains(currentState);

  def readCharacter(ch: Char) {
    currentState = rulebook.nextState(currentState, ch);
  }

  def readString(str: String) {
    str.foreach(readCharacter);
  }

}

implicit class Tappable[A](a: A) {
  def tap[B](p: A => B): A = { p(a); a }
}

case class DFADesign (startState: State, acceptStates: Seq[State], rulebook: DFARulebook) {

  def toDFA = new DFA(startState, acceptStates, rulebook);

  def accepts(str: String): Boolean = toDFA.tap(_.readString(str)).accepting;

}

// scala> val rulebook = DFARulebook(List(FARule(1, 'a', 2), FARule(1, 'b', 1), FARule(2, 'a', 2), FARule(2, 'b', 3),  FARule(3, 'a', 3), FARule(3, 'b', 3)))


// $ scala -i ./3.1.4.scala
// scala> val rulebook = DFARulebook(List(
//      | FARule(1, 'a', 2), FARule(1, 'b', 1),
//      | FARule(2, 'a', 2), FARule(2, 'b', 3),
//      | FARule(3, 'a', 3), FARule(3, 'b', 3)))
// rulebook: DFARulebook = DFARulebook(List(#<FARule 1 --a--> 2>, #<FARule 1 --b--> 1>, #<FARule 2 --a--> 2>, #<FARule 2 --b--> 3>, #<FARule 3 --a--> 3>, #<FARule 3 --b--> 3>))
// 
// scala> rulebook.nextState(1, 'a')
// res0: State = 2
// 
// scala> rulebook.nextState(1, 'b')
// res1: State = 1
// 
// scala> rulebook.nextState(2, 'b')
// res2: State = 3
// 
// scala> val dfa = new DFA(1, List(1, 3), rulebook)
// dfa: DFA = DFA@312de3fa
// 
// scala> dfa.accepting
// res3: Boolean = true
// 
// scala> val dfa = new DFA(1, List(3), rulebook)
// dfa: DFA = DFA@6efce31f
// 
// scala> dfa.accepting
// res4: Boolean = false
// 
// scala> dfa.readString("baaab"); dfa.accepting
// res8: Boolean = true
// 
// scala> val dfaDesign = DFADesign(1, List(3), rulebook)
// dfaDesign: DFADesign = DFADesign(1,List(3),DFARulebook(List(#<FARule 1 --a--> 2>, #<FARule 1 --b--> 1>, #<FARule 2 --a--> 2>, #<FARule 2 --b--> 3>, #<FARule 3 --a--> 3>, #<FARule 3 --b--> 3>)))
// 
// scala> dfaDesign.accepts("a")
// res9: Boolean = false
// 
// scala> dfaDesign.accepts("baa")
// res10: Boolean = false
// 
// scala> dfaDesign.accepts("baba")
// res11: Boolean = true


