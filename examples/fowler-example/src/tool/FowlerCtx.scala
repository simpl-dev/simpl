package ee.cyber.simplicitas.fowlerdsl

import ee.cyber.simplicitas._

class FowlerCtx(val tree: Program) {
  import collection.mutable.{Map, ArrayBuffer}
  
  val events = Map[String, Event]()
  val commands = Map[String, Command]()
  val states = Map[String, State]()
  
  var errors = new ArrayBuffer[SourceMessage]()

  makeSymbolTable()
  
  def makeSymbolTable() {
    tree.machines foreach makeSymbolTable
    
//    println("Events: " + events);
//    println("Commands: " + commands);
//    println("States: " + states);
  }

  def makeSymbolTable(machine: Machine) {
    type Named = { def name: Id }
    def doAdd[T <: Named](map: Map[String, T])(item: T) {
      map += (item.name.text -> item)
    }

    machine.events.events foreach doAdd(events)
    machine.commands.commands foreach doAdd(commands)
    machine.states foreach doAdd(states)
  }
}