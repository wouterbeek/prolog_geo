:- module(
  wkt,
  [
    is_wkt_shape/1 % @Term
  ]
).

/** <module> Well-Known Text (WKT)

@author Wouter Beek
@version 2017-2018
*/

:- reexport(library(gis/wkt_generate)).
:- reexport(library(gis/wkt_parser)).





%! is_wkt_shape(@Term) is semidet.

is_wkt_shape('CircularString'(_)).
is_wkt_shape('CompoundCurve'(_)).
is_wkt_shape('CurvePolygon'(_)).
is_wkt_shape('GeometryCollection'(_)).
is_wkt_shape('LineString'(_)).
is_wkt_shape('MultiCurve'(_)).
is_wkt_shape('MultiLineString'(_)).
is_wkt_shape('MultiPoint'(_)).
is_wkt_shape('MultiPolygon'(_)).
is_wkt_shape('MultiSurface'(_)).
is_wkt_shape('Point'(_)).
is_wkt_shape('Polygon'(_)).
is_wkt_shape('PolyhedralSurface'(_)).
is_wkt_shape('TIN'(_)).
is_wkt_shape('Triangle'(_)).
