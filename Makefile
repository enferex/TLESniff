APP = tlesniff

all: $(APP)

$(APP): Main.hs TLE/*.hs
	ghc -static --make Main.hs -o $@ 

clean:
	$(RM) *.{hi,o} $(APP) TLE/*.{hi,o}
