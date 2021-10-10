all: tlesniff

tlesniff: Main.hs TLE/*.hs
	stack install --silent

clean:
	stack purge
	$(RM) tlesniff
