package ee.cyber.simplicitas.parse

import collection.mutable.ArrayBuffer

class LexerState {
    def enter(state: Int) {
        stack += state
    }

    def exit(state: Array[Int]) {
        while (!stack.isEmpty) {
            if (state.contains(top)) {
                pop
                return
            }

            pop
        }
    }

    def check(state: Array[Int]) =
        state.contains(top)

    private def top: Int =
        stack(stack.size - 1)

    private def pop {
        stack.remove(stack.size - 1)
    }

    private var stack = ArrayBuffer[Int]()
}