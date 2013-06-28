// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import collection.mutable.ArrayBuffer

/** Helper class to manage stack of lexer states. Lexer states are
 * represented as integers. Managing the mapping from states to integers is
 * reponsibility of the generated ANTLR grammar. */
class LexerState {
    // The stack of lexer states.
    private var stack = ArrayBuffer[Int]()

    def enter(state: Array[Int]) {
        state.foreach(add)
    }

    private def add(item: Int) {
        if (stack.isEmpty || (top != item)) {
            stack += item
        }
    }

    def exit(state: Array[Int]) {
        while (!stack.isEmpty) {
            if (state.contains(top)) {
                pop()
                return
            }

            pop()
        }
    }

    def checkAll(state: Array[Int]) =
        state.forall(stack.contains)

    def checkAny(state: Array[Int]) =
        state.exists(stack.contains)

    def checkNone(state: Array[Int]) =
        !checkAny(state)

    private def top: Int =
        stack(stack.size - 1)

    private def pop() {
        stack.remove(stack.size - 1)
    }
}