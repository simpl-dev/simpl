// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.ide;

import ee.cyber.simplicitas._

class DocGenerator(destDir: String)
        extends GeneratorBase(destDir)  {
	var docText: StringBuilder = null
	val lf = "\n"
	val ws = " "
 	val backSlash = "\\"
	val colon = ":"
 	val semicolon = ";"
 	val pipe = " | "
 	val openingBracket = "("
 	val closinBracket = ")"
    val bold = "**"

	def generate(tree: GrammarDef, fileName: String) = {
 		docText = new StringBuilder()
		genDoc(tree)
 		writeFile(fileName + ".rst", docText.toString)
	}

	def genDoc(tree: GrammarDef) {
		tree.rules.foreach(addRuleDef)
	}

	def appendToDoc(text: String) {
		if (text == closinBracket) {
			removeWsFromEnd()
		}
		docText.append(text)
	}
 
	def addRuleDef(rd: RuleDef) {
		rd match {
        	case rule:TerminalDef => 
        		addTerminalDef(rule)
        	case rule:FragmentDef =>
        		addFragmentDef(rule)
        	case rule:OptionDef =>
        		addOptionDef(rule)
        	case rule:NonTerminalDef =>
        		addNonTerminalDef(rule)
        	case _ => ()// XXX do something?
		}
	}
  
	def addTerminalDef(t: TerminalDef) {
		addRuleDef(t.name, t.documentation)
		appendToDoc(colon)
		addWs()
		addPattern(t.pattern)
		addSemicolon()
		appendToDoc(lf)
	}

	def addFragmentDef(f: FragmentDef) {
		addRuleDef(f.name, f.documentation)
		appendToDoc(colon)
		addWs()
		addPattern(f.pattern)
		addSemicolon()
		appendToDoc(lf)
	}
  
	def addOptionDef(o: OptionDef) {
		addRuleDef(o.name, o.documentation)
		appendToDoc(colon)
		addWs()
		// alts
		if (o.alts != null) {
			addDefLink(o.alts.id)
		}
		// alts2
		addRuleList(o.alts2)

		addSemicolon()
		appendToDoc(lf)
	}
	
	def addRuleList(ruleList: List[CommonNode]) {
		if (ruleList != null) {
			if (!ruleList.isEmpty) {
				appendToDoc(pipe)
			}
			for (i <- 0 until ruleList.length) {
				ruleList(i) match {
	        		case r:RuleRef =>
	        			addDefLink(r.id)
	        		case r:MatchList =>
	        			addMatchList(r)
	        		case r:TermList =>
	        			addTermList(r)
	        	}
				if (i < ruleList.length - 1) {
					appendToDoc(pipe)
				}
			}
		}
	}

	def addNonTerminalDef(ntd: NonTerminalDef) {
		addRuleDef(ntd.name, ntd.documentation)
		appendToDoc(colon)
		addWs()
		addAltList(ntd.alt)
		addSemicolon()
		appendToDoc(lf)
	}

	def addAltList(aList: AltList) {
		if (aList != null) {
			if (aList.lst != null) {
				addMatchList(aList.lst)
			}
			// lst2
			addRuleList(aList.lst2)
		}
	}
  
	def addMatchList(mList: MatchList) {
		mList.m.foreach(addMatch)
	}
  
	def addMatch(m: Match) {
		addMatchToken(m.token)
		addModifier(m.modifier)
	}
  
	def addMatchToken(token: MatchToken) {
		if (token.alt != null && token.alt.length > 0) {
			appendToDoc(openingBracket)
			addAltList(token.alt)
			appendToDoc(closinBracket)
		}
		if (token.ref != null) {
			addDefLink(token.ref.id)
		}
		if (token.str != null) {
			addWs()
			appendToDoc(bold)
			appendToDoc(token.str.text)
            appendToDoc(bold)
			addWs()
		}
	}
  
	def addPattern(pattern: Pattern) {
		// lst
		if (pattern.lst != null) {
			addTermList(pattern.lst)
		}
		// lst2
		addRuleList(pattern.lst2)
	}
  
	def addTermList(lst: TermList) {
		lst.termMatch.foreach(addTermMatch)
	}
  
	def addTermMatch(tMatch: TermMatch) {
		if (tMatch.inv != null) {
			addTilde(tMatch.inv)
		}
		if (tMatch.t != null) {
			addTerminal(tMatch.t)
		}
		addModifier(tMatch.modifier)
        addWs()
	}
  
	def addTilde(t: Tilde) {
		appendToDoc(t.text)
	}
  
	def addTerminal(terminal: Terminal) {
		terminal match {
        	case t:TerminalList =>
        		addTerminalList(t)
        	case t:FragmentRef =>
        		addFragmentRef(t)
        	case t:TerminalRange =>
        		addTerminalRange(t)
        	case t:TerminalLiteral =>
        		addTerminalLiteral(t)
        	case t:TerminalDot =>
        		addTerminalDot(t)
        	case _ => () // XXX: log something?
		}    
	}

	def addTerminalList(tList: TerminalList) {
		appendToDoc(openingBracket)
		addPattern(tList.pattern)
		appendToDoc(closinBracket)
	}
  
	def addTerminalDot(td: TerminalDot) {
		// XXX dot?
		appendToDoc(".")
	}

	def addTerminalRange(tr: TerminalRange) {
		appendToDoc(tr.start.text + ".." + tr.end.text)
	}
  
	def addFragmentRef(f: FragmentRef) {
		addDefLink(f.id)
	}

	def addTerminalLiteral(l: TerminalLiteral) {
		appendToDoc(l.str.text
                .replaceAll("""\\""","""\\\\""")
                .replaceAll("\\*","""\\*"""))
	}
  
	def addRuleDef(id: Id, comment: String) {
		appendToDoc(lf)
		appendToDoc(".. _" + id.text + ":") // create anchor's ID
		appendToDoc(lf * 2)
		if (comment != null) {
            // RST does not like newlines inside emphasised text.      
			appendToDoc("*" + comment.replace('\n', ' ') + "*")
			appendToDoc(lf * 2)
		}
		appendToDoc(id.text)
	}
 
	def addDefLink(id: Id) {
		addWs()
		appendToDoc(id.text + "_") // create link
		addWs()
	}
 
	def addModifier(modifier: Modifier) {
		if (modifier != null) {
			removeWsFromEnd()
			appendToDoc(backSlash + modifier.text)
		}
	}
 
	def addSemicolon() {
		removeWsFromEnd()
		appendToDoc(semicolon)
	}
	
	def removeWsFromEnd() {
		if (docText.endsWith(ws)) {
			docText.deleteCharAt(docText.length - 1)
		}
	}
	
	def addWs() {
		if (!docText.endsWith(ws) && !docText.endsWith(openingBracket)) {
			appendToDoc(ws)
		}
	}
 }


