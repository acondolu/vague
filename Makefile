src/Vague/Parser/Lexer.hs: src/Vague/Parser/gen/Lexer.x
	alex src/Vague/Parser/gen/Lexer.x -o src/Vague/Parser/Lexer.hs
	# ormolu -i src/Vague/Parser/Lexer.hs # ormolu does not support CPP, unfortunately

src/Vague/Parser/Happy.hs: src/Vague/Parser/gen/Parser.y
	happy src/Vague/Parser/gen/Parser.y -o src/Vague/Parser/Happy.hs
	ormolu -i src/Vague/Parser/Happy.hs

.PHONY := format-haskell

format-haskell:
	find src -name "*.hs" -exec ormolu -i {} \;
	find test -name "*.hs" -exec ormolu -i {} \;
