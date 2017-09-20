default: bevan

bevan: Main.hs Grok.hs Storage.hs Dent.hs Events.hs Sig.hs QuizUtils.hs ExParse.hs BinRep.hs HuffTree.hs BinArith.lhs CVI.lhs CVICompile.lhs CVIFront.lhs CVIParse.lhs LLL.lhs Q9.lhs TINY.lhs TINYAss.lhs TINYQuizKit.lhs
	ghc --make Main.hs -o bevan
