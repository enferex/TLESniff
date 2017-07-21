### TLESniff: A Two Line Element Parser

#### What
Two Line Elements are published ASCII satellite position information.
TLESniff can be used to read the raw (ASCII) text files that represent TLE data.
TLESniff will then publish that data into a local sqlite database.

#### Dependencies
* Haskell runtime
* Database.SQLite package for Haskell
* Network.Curl package for Haskell (for reading remotely published TLE data)

#### Build
Simply run `make` to build.  Note that the data sources in Net.hs have been
removed.  Data sources should point diretly to ASCII TLE data, see celestrak.com
for updated ASCII data sources.

#### Resources
* https://www.celestrak.com/NORAD/elements/
* https://www.space-track.org/documentation#/tle
* Lots of googling and Haskell docs.

#### Contact
mattdavis9@gmail.com (enferex)
