### TLESniff: A Two Line Element Parser

#### What
Two Line Elements are ASCII data representing satellite position information.
TLESniff can be used to read the raw (ASCII) text files that represent TLE data.
TLESniff will then publish that data into a local sqlite database.

#### Dependencies
* Haskell runtime
* Database.SQLite package for Haskell
* Network.Curl package for Haskell (for reading remotely published TLE data)

#### Notes
* The data sources in `Net.hs` have been removed.  Data sources should point
diretly to ASCII TLE data, see celestrak.com for updated ASCII data sources.

#### Build
Simply run `make` to build.  

#### Resources
* https://www.celestrak.com/NORAD/elements/
* https://www.space-track.org/documentation#/tle
* https://spaceflight.nasa.gov/realdata/sightings/SSapplications/Post/JavaSSOP/SSOP_Help/tle_def.html
* https://en.wikipedia.org/wiki/Two-line_element_set
* Lots of googling and Haskell docs.


#### Contact
Matt Davis (https://github.com/enferex)
