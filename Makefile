.PHONY: all clean

TARGETS = Robotik

all: $(TARGETS)

%: %.hs
	ghc --make $<

clean:
	$(RM) *.hi *.o $(TARGETS)
