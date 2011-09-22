package ee.cyber.simplicitas.prettyprint

// Adopted from PPrint library by Dan Leijen.
// Original copyright:

// Copyright 2000, Daan Leijen. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the
//   documentation and/or other materials provided with the distribution.
//
// This software is provided by the copyright holders “as is” and any
// express or implied warranties, including, but not limited to, the implied
// warranties of merchantability and fitness for a particular purpose are
// disclaimed. In no event shall the copyright holders be liable for any
// direct, indirect, incidental, special, exemplary, or consequential
// damages (including, but not limited to, procurement of substitute
// goods or services; loss of use, data, or profits; or business
// interruption) however caused and on any theory of liability, whether
// in contract, strict liability, or tort (including negligence or
// otherwise) arising in any way out of the use of this software, even if
// advised of the possibility of such damage.

import java.io.Writer

object Doc {
    val empty = Empty

    def char(c: Char) = c match {
        case '\n' => line
        case _ => DChar(c)
    }

    implicit def text(s: String): Doc = s match {
        case "" => empty
        case _ => Text(s)
    }

    val line = Line(false)
    val linebreak = Line(true)

    val lparen = char('(')
    val rparen = char(')')
    val langle = char('<')
    val rangle = char('>')
    val lbrace = char('{')
    val rbrace = char('}')
    val lbracket = char('[')
    val rbracket = char(']')

    val squote = char('\'')
    val dquote = char('"')
    val semi = char(';')
    val colon = char(':')
    val comma = char(',')
    val space = char(' ')
    val dot = char('.')
    val backslash = char('\\')
    val equals = char('=')

    val sep = (group _ compose vsep)
    val fillSep = fold(_ :|: _) _
    val hsep = fold(_ :+: _) _
    val vsep = fold(_ :#: _) _

    def cat(x: List[Doc]) = group(vcat(x))
    val fillCat = fold(_ :||: _) _
    val hcat = fold(_ :: _) _
    val vcat = fold(_ :##: _) _

    def fold(f: (Doc, Doc) => Doc)(lst: List[Doc]) = lst match {
        case Nil => empty
        case lst => lst.reduceLeft(f)
    }

    def punctuate(sep: Doc , items: List[Doc]): Doc = {
        def loop(lst: List[Doc]): Doc = lst match {
            case Nil => empty
            case List(d) => d
            case h :: t => h :: sep :: loop(t)
        }

       loop(items)
    }

    def withCommas(lst: List[Doc]): Doc =
        punctuate(comma :: space, lst)

    val softline = group(line)
    val softbreak = group(linebreak)

    val squotes = enclose(squote, squote) _
    val dquotes = enclose(dquote, dquote) _
    val braces = enclose(lbrace, rbrace) _
    val parens = enclose(lparen, rparen) _
    val angles = enclose(langle, rangle) _
    val brackets = enclose(lbracket, rbracket) _
    def enclose(l: Doc, r: Doc)(x: Doc) = l :: x :: r

    def beside(l: Doc, r: Doc) = Cat(l, r)
    def nest(indent: Int, doc: Doc) = Nest(indent, doc)
    def column(f: Int => Doc) = Column(f)
    def nesting(f: Int => Doc) = Nesting(f)
    def group(x: Doc) = Union(flatten(x), x)

    private def flatten(doc: Doc): Doc = doc match {
        case Cat(x, y) => Cat(flatten(x), flatten(y))
        case Nest(i, x) => Nest(i, flatten(x))
        case Line(break) => if (break) empty else Text(" ")
        case Union(x, y) => flatten(x)
        case Column(f) => Column(flatten _ compose f)
        case Nesting(f) => Nesting(flatten _  compose f)
        case _ => doc
    }

    def fillBreak(f: Int, x: Doc) = {
        def fun(w: Int) =
            if (w > f)
                nest(f, linebreak)
            else
                text(spaces(f - w))

        width(x, fun)
    }

    def fill(f: Int, d: Doc) = {
        def fun(w: Int) =
            if (w >= f)
                nest(f, linebreak)
            else
                text(spaces(f - w))

        width(d, fun)
    }

    def width(d: Doc, fun: Int => Doc) = {
        def fun(k1: Int): Doc =
            d :: column(k2 => fun(k2 - k1))

        column(fun)
    }

    def indent(i: Int, d: Doc) = hang(i, text(spaces(i)) :: d)
    def hang(i: Int, d: Doc) = align(nest(i, d))
    def align(d: Doc) = {
        def fun(k: Int) =
            nesting(k2 => nest(k - k2, d))

        column(fun)
    }

    // Renderers
    private def renderPretty(ribbonFrac: Double, width: Int,
                             doc: Doc): SimpleDoc = {
        //r :: the ribbon width in characters
        val r = 0 max (width min ((width * ribbonFrac) round).toInt)

        // nicest :: r = ribbon width, w = page width,
        //           n = indentation of current line, k = current column
        //           x and y, the (simple) documents to chose from.
        //           precondition: first lines of x are longer than the
        //           first lines of y.
        def nicest(n: Int, k: Int, x: SimpleDoc, y: SimpleDoc) = {
            val w = (width - k) min (r - k + n)

            if (fits(w, x))
                x
            else
                y
        }

        // best :: n = indentation of current line
        //         k = current column
        //         (ie. (k >= n) && (k - n == count of inserted characters)
        def best(n: Int, k: Int, d: Docs): SimpleDoc = d match {
            case DNil => SEmpty
            case DCons(i, d, ds) => d match {
                case Empty => best(n, k, ds)
                case DChar(c) => SChar(c, best(n, k + 1, ds))
                case Text(s) => SText(s, best(n, k + 1, ds))
                case Line(_) => SLine(i, best(i, i, ds))
                case Cat(x, y) => best(n, k, DCons(i, x, DCons(i, y, ds)))
                case Nest(j, x) => best(n, k, DCons(i + j, x, ds))
                case Union(x, y) =>
                    nicest(n, k,
                        best(n, k, DCons(i, x, ds)),
                        best(n, k, DCons(i, y, ds)))
                case Column(f) => best(n, k, DCons(i, f(k), ds))
                case Nesting(f) => best(n, k, DCons(i, f(i), ds))
            }
        }

        best(0, 0, DCons(0, doc, DNil))
    }

    private def fits(w: Int, x: SimpleDoc): Boolean =
        if (w < 0)
            false
        else x match {
            case SEmpty => true
            case SChar(c, x) => fits(w - 1, x)
            case SText(s, x) => fits(w - s.length, x)
            case SLine(_, _) => true
        }

    def show(doc: Doc, width: Int): String = {
        val writer = new java.io.StringWriter
        show(doc, 0.4, width, writer)
        writer.toString
    }

    def show(doc: Doc, rfrac: Double, width: Int, writer: Writer) {
        def display(d: SimpleDoc): Unit = d match {
            case SEmpty => ()
            case SChar(c, x) =>
                writer.write(c)
                display(x)
            case SText(s, x) =>
                writer.write(s)
                display(x)
            case SLine(i, x) =>
                writer.write('\n')
                writer.write(spaces(i))
                display(x)
        }

        val sdoc = renderPretty(rfrac, width, doc)
        display(sdoc)
    }

    private def spaces(n: Int) = {
        val ret = new StringBuilder

        var rem = n
        while (rem >= 16) { ret append "                "; rem -= 16 }
        if (rem >= 8)     { ret append "        "; rem -= 8 }
        if (rem >= 4)     { ret append "    "; rem -= 4 }
        if (rem >= 2)     { ret append "  "; rem -= 2}
        if (rem == 1)     { ret append " " }

        ret.toString
    }
}

sealed trait Doc {
    import Doc._

    override def toString = Doc.show(this, 70)

    def ::(x: Doc) = beside(x, this)
    def :+:(x: Doc) = x :: space :: this
    def :|:(x: Doc) = x :: softline :: this
    def :||:(x: Doc) = x :: softbreak :: this
    def :#:(x: Doc) = x :: line :: this
    def :##:(x: Doc) = x :: linebreak :: this
}

case object Empty extends Doc
case class DChar(c: Char) extends Doc
case class Text(s: String) extends Doc
case class Line(isHard: Boolean) extends Doc
case class Cat(l: Doc, r: Doc) extends Doc
case class Nest(indent: Int, doc: Doc) extends Doc
case class Union(x: Doc, y: Doc) extends Doc
case class Column(f: Int => Doc) extends Doc
case class Nesting(f: Int => Doc) extends Doc

abstract class SimpleDoc
case object SEmpty extends SimpleDoc
case class SChar(c: Char, doc: SimpleDoc) extends SimpleDoc
case class SText(s: String, doc: SimpleDoc) extends SimpleDoc
case class SLine(i: Int, doc: SimpleDoc) extends SimpleDoc

abstract class Docs
case object DNil extends Docs
case class DCons(i: Int, d: Doc, ds: Docs) extends Docs