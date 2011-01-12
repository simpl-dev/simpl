package ee.cyber.simplicitas.parse

import collection.mutable.ArrayBuffer

class LexerState {
    def enter(state: Int) {
        if (stack.isEmpty || (top != state)) {
            stack += state
        }
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

    def checkTop(state: Array[Int]) =
        !stack.isEmpty && stack.contains(top)

    def checkAny(state: Array[Int]) =
        state.exists(stack.contains)

    private def top: Int =
        stack(stack.size - 1)

    private def pop {
        stack.remove(stack.size - 1)
    }

    private var stack = ArrayBuffer[Int]()
}