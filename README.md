### TLESniff: A Two Line Element Parser

#### What
Two Line Elements are ASCII data representing satellite position information.
TLESniff can be used to read the raw (ASCII) text files that represent TLE data.
TLESniff will then publish that data into a local sqlite database.

#### Dependencies
* Haskell Stack - https://www.haskellstack.org

#### Notes
The data sources in `Net.hs` have been removed.  Data sources should point
directly to ASCII TLE data, see [celestrak.com](https://www.celestrak.com) and
[tle.info](https://www.tle.info) for updated ASCII data sources.

tlesniff takes a textfile as input (--url option).  Each line in that text file
should be a URL to TLE data; see the sites mentioned above.  Those sites provide
numerous TLE sources to choose from.

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
