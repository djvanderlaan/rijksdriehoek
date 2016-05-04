rijksdriehoek
=============

Convert Dutch National Grid (Rijksdriehoek) coordinates from and to WGS84 coordinates. It uses the
<a href="http://www.dekoepel.nl/pdf/Transformatieformules.pdf">approximation equations by F.H. 
Sleutelkamp and G.L. Strang van Hees</a>.


Javascript
----------

The javascript file in the js directory defines a `d3.geo.rijksdriehoek` projection that can be 
used as regular projections in d3 to project maps in WGS84 coordinates to Dutch National Grid
coordinates. For an example see <a href="http://www.iostream.nl/pages/rijksdriehoek/">rijksdriehoek</a>.

R
-

The R file contains two functions to project from WGS84 to Dutch National Grid and vice versa. It
furthermore defines a function that can change the projection of a geoJSON file.


Python
------

Tests are included in the file (run `python rijksdriehoek.py`). Available functions are `rd_to_wgs(x,y)`
and `wgs_to_rd(phi, lambda)`.

