GHC = ghc

MAIN = hessu

all: $(MAIN)

$(MAIN):
	$(GHC) --make Main -o $(MAIN)

clean:
	del *.o
	del *.hi