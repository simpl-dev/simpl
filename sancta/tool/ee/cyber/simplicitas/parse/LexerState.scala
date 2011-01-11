package ee.cyber.simplicitas.parse

import collection.mutable.ArrayBuffer

class LexerState {
    def enter(state: Int): Unit = {
        stack += state
    }

    def exit(state: Array[Int]): Unit = {
        while (!stack.isEmpty) {
            if (state.contains(top)) {
                pop
                return
            }

            pop
        }
    }

    def check(state: Array[Int]): Boolean = {
        state.contains(top)
    }

    private def top: Int = {
        return stack(stack.size - 1)
    }

    private def pop: Unit = {
        stack.remove(stack.size - 1)
    }

    private var stack = ArrayBuffer[Int]()
}