build:
	ghc --make Main.hs -o tangle -dynamic
clean:
	@echo cleaning...
	@rm *.hi *.o tangle
	@echo done!

