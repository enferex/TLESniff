APP = tlesniff

all: $(APP)

$(APP): $(SRCS)
	ghc --make Main.hs -o $@

clean:
	$(RM) *.{hi,o} $(APP) TLE/*.{hi,o}
