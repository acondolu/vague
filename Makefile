.PHONY := format-haskell

format-haskell:
	find src -name "*.hs" -exec ormolu -i {} \;
	find test -name "*.hs" -exec ormolu -i {} \;
	find app -name "*.hs" -exec ormolu -i {} \;

