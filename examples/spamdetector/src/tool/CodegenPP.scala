package ee.cyber.simplicitas.spamdetector

import java.io.Writer
import ee.cyber.simplicitas.prettyprint.Doc
import Doc._

object CodegenPP {
    def prettyPrint(program: Program, writer: Writer) {
        val doc = prettyPrint(program)
        show(doc, 0.8, 75, writer)
    }

    def toString(program: Program) = {
        val doc = prettyPrint(program)
        doc.toString
    }

    private def prettyPrint(program: Program): Doc =
        "class Detector extends DetectorBase {" :#:
            indent(hcat(program.items map itemDef)) :#:
            indent(
                "public void run() {" :#:
                    indent(hcat(program.items map itemCall)) :#:
                text("}")) :#:
        text("}")

    private def itemDef(item: Item) = item match {
        case Condition(Id(name), expr) =>
            "private boolean" :+: name :: "() {" :#:
                indent(hang(8, "return" :+: prettyPrint(expr) :: ";")) :#:
            "}" :: line
        case _ => empty
    }

    private def itemCall(item: Item) = item match {
        case Rule(Name(name), Score(score), expr) =>
            hang(8, "if" :+: parens(prettyPrint(expr)) :+: "{") :#:
                indent("addMatch(" :: name ::"," :+: score :: ");") :#:
            "}" :: line
        case _ => empty
    }

    private def prettyPrint(expr: Expression): Doc = expr match {
        case OrExpression(items) => withOperator(" || ", items)
        case AndExpression(items) => withOperator(" && ", items)
        case ConditionCall(Id(name)) => text(name) :: text("()")
        case Contains(Id(field), Regexp(regexp)) =>
            "fieldContains" :: parens(field :: comma :+: text(regexp))
        case NotContains(Id(field), Regexp(regexp)) =>
            "!fieldContains" :: parens(field :: comma :+: text(regexp))
        case NotExpression(expr) => "!" :: parens(prettyPrint(expr))
        case Count(ExprList(items), Num(count)) =>
            "countOf" ::
                    parens("new boolean[]" :+:
                        braces(withCommas(items map prettyPrint))) :+:
                "=" :+: text(count)
        case _ => empty
    }

    private def withOperator(op: String, exprList: List[Expression]) =
        cat(punctuate(text(op), exprList map prettyPrint))

    private val indent: Doc => Doc = Doc.indent(4, _)
}