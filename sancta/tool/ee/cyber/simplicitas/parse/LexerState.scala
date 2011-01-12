package ee.cyber.simplicitas.parse

import collection.mutable.ArrayBuffer

class LexerState {
    def enter(state: Array[Int]) {
        // TODO: check for duplicates.
        stack ++= state
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

    def checkAll(state: Array[Int]) =
        state.forall(stack.contains)

    def checkAny(state: Array[Int]) =
        state.exists(stack.contains)

    private def top: Int =
        stack(stack.size - 1)

    private def pop {
        stack.remove(stack.size - 1)
    }

    private var stack = ArrayBuffer[Int]()
}