SRCS = Main.hs TLE.hs Net.hs Database.hs
HCC  = ghc
APP  = tlesniff

all: $(APP)

$(APP): $(SRCS)
	$(HCC) $^ -o $@

clean:
	$(RM) *.hi *.o $(APP)
