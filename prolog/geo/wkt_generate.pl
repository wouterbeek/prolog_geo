:- module(
  wkt_generate,
  [
    wkt_generate//1 % +Shape:compound
  ]
).

/** <module> Well-Known Text (WKT): Generator

@author Wouter Beek
@version 2016-2018
*/

:- use_module(library(error)).

:- use_module(library(dcg)).
:- use_module(library(default)).

:- meta_predicate
    'wkt+'(3, -, ?, ?),
    'wkt*'(3, -, ?, ?).





% CircularString

circularstring_text(_, _, 'CircularString'([])) --> !,
  "Empty".
circularstring_text(Z, LRS, 'CircularString'(Points)) -->
  "(",
  'wkt+'(point(Z, LRS), Points),
  ")".

circularstring_text_representation(Z, LRS, CircularString) -->
  "CircularString",
  z_m(Z, LRS),
  circularstring_text(Z, LRS, CircularString).



collection_text_representation(Z, LRS, MultiPoint) -->
  multipoint_text_representation(Z, LRS, MultiPoint), !.
collection_text_representation(Z, LRS, MultiCurve) -->
  multicurve_text_representation(Z, LRS, MultiCurve), !.
collection_text_representation(Z, LRS, MultiSurface) -->
  multisurface_text_representation(Z, LRS, MultiSurface), !.
collection_text_representation(Z, LRS, GeometryCollection) -->
  geometrycollection_text_representation(Z, LRS, GeometryCollection).



% CompoundCurve

compoundcurve_text(_, _, 'CompoundCurve'([])) --> !,
  "Empty".
compoundcurve_text(Z, LRS, 'CompoundCurve'(Curves)) -->
  "(",
  'wkt+'(single_curve_text(Z, LRS), Curves),
  ")".

compoundcurve_text_representation(Z, LRS, CompoundCurve) -->
  "CompoundCurve",
  z_m(Z, LRS),
  compoundcurve_text(Z, LRS, CompoundCurve).



curve_text(Z, LRS, LineString) -->
  linestring_text_body(Z, LRS, LineString), !.
curve_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
curve_text(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).

curve_text_representation(Z, LRS, LineString) -->
  linestring_text_representation(Z, LRS, LineString), !.
curve_text_representation(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
curve_text_representation(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).



% CurvePolygon

curvepolygon_text(_, _, 'CurvePolygon'([])) --> !,
  "Empty".
curvepolygon_text(Z, LRS, 'CurvePolygon'(Rings)) -->
  "(",
  'wkt+'(ring_text(Z, LRS), Rings),
  ")".

curvepolygon_text_body(Z, LRS, CurvePolygon) -->
  curvepolygon_text(Z, LRS, CurvePolygon).

curvepolygon_text_representation(Z, LRS, CurvePolygon) -->
  "CurvePolygon",
  z_m(Z, LRS),
  curvepolygon_text_body(Z, LRS, CurvePolygon), !.
curvepolygon_text_representation(Z, LRS, Polygon) -->
  polygon_text_representation(Z, LRS, Polygon), !.
curvepolygon_text_representation(Z, LRS, Triangle) -->
  triangle_text_representation(Z, LRS, Triangle).



% GeometryCollection

geometrycollection_text(_, _, 'GeometryCollection'([])) --> !,
  "Empty".
geometrycollection_text(Z, LRS, 'GeometryCollection'(Shapes)) -->
  "(",
  'wkt+'(wkt_representation(Z, LRS), Shapes),
  ")".

geometrycollection_text_representation(Z, LRS, GeometryCollection) -->
  "GeometryCollection",
  z_m(Z, LRS),
  geometrycollection_text(Z, LRS, GeometryCollection).



% LineString

linestring_text(_, _, 'LineString'([])) --> !,
  "Empty".
linestring_text(Z, LRS, 'LineString'(Points)) -->
  "(",
  'wkt+'(point(Z, LRS), Points),
  ")".

linestring_text_body(Z, LRS, LineString) -->
  linestring_text(Z, LRS, LineString).

linestring_text_representation(Z, LRS, LineString) -->
  "LineString",
  z_m(Z, LRS),
  linestring_text_body(Z, LRS, LineString).



% MultiCurve

multicurve_text(_, _, 'MultiCurve'([])) --> !,
  "Empty".
multicurve_text(Z, LRS, 'MultiCurve'(Curves)) -->
  "(",
  'wkt+'(curve_text(Z, LRS), Curves),
  ")".

multicurve_text_representation(Z, LRS, MultiCurve) -->
  "MultiCurve",
  z_m(Z, LRS),
  multicurve_text(Z, LRS, MultiCurve), !.
multicurve_text_representation(Z, LRS, MultiLineString) -->
  multilinestring_text_representation(Z, LRS, MultiLineString).



% MultiLineString

multilinestring_text(_, _, 'MultiLineString'([])) --> !,
  "Empty".
multilinestring_text(Z, LRS, 'MultiLineString'(LineStrings)) -->
  "(",
  'wkt+'(linestring_text_body(Z, LRS), LineStrings),
  ")".

multilinestring_text_representation(Z, LRS, MultiLineString) -->
  "MultiLineString",
  z_m(Z, LRS),
  multilinestring_text(Z, LRS, MultiLineString).



% MultiPoint

multipoint_text(_, _, 'MultiPoint'([])) --> !,
  "Empty".
multipoint_text(Z, LRS, 'MultiPoint'(Points)) -->
  "(",
  'wkt+'(point(Z, LRS), Points),
  ")".

multipoint_text_representation(Z, LRS, MultiPoint) -->
  "MultiPoint",
  z_m(Z, LRS),
  multipoint_text(Z, LRS, MultiPoint).



% MultiPolygon

multipolygon_text(_, _, 'MultiPolygon'([])) --> !,
  "Empty".
multipolygon_text(Z, LRS, 'MultiPolygon'(Polygons)) -->
  "(",
  'wkt+'(polygon_text_body(Z, LRS), Polygons),
  ")".

multipolygon_text_representation(Z, LRS, MultiPolygon) -->
  "MultiPolygon",
  z_m(Z, LRS),
  multipolygon_text(Z, LRS, MultiPolygon).



% MultiSurface

multisurface_text(_, _, 'MultiSurface'([])) --> !,
  "Empty".
multisurface_text(Z, LRS, 'MultiSurface'(Surfaces)) -->
  "(",
  'wkt+'(surface_text(Z, LRS), Surfaces),
  ")".

multisurface_text_representation(Z, LRS, MultiSurface) -->
  "MultiSurface",
  z_m(Z, LRS),
  multisurface_text(Z, LRS, MultiSurface), !.
multisurface_text_representation(Z, LRS, MultiPolygon) -->
  multipolygon_text_representation(Z, LRS, MultiPolygon), !.
multisurface_text_representation(Z, LRS, PolyhedralSurface) -->
  polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface), !.
multisurface_text_representation(Z, LRS, Tin) -->
  tin_text_representation(Z, LRS, Tin).



% Point

point_text(_, _, 'Point'([])) --> !,
  "Empty".
point_text(Z, LRS, 'Point'(Coords)) -->
  "(",
  point(Z, LRS, 'Point'(Coords)),
  ")".

point_text_representation(Z, LRS, Point) -->
  "Point",
  z_m(Z, LRS),
  point_text(Z, LRS, Point).



% Polygon

polygon_text(_, _, 'Polygon'([])) --> !,
  "Empty".
polygon_text(Z, LRS, 'Polygon'(LineStrings)) -->
  "(",
  'wkt+'(linestring_text(Z, LRS), LineStrings),
  ")".

polygon_text_body(Z, LRS, Polygon) -->
  polygon_text(Z, LRS, Polygon).

polygon_text_representation(Z, LRS, Polygon) -->
  "Polygon",
  z_m(Z, LRS),
  polygon_text_body(Z, LRS, Polygon).



% PolyhedralSurface

polyhedralsurface_text(_, _, 'PolyhedralSurface'([])) --> !,
  "Empty".
polyhedralsurface_text(Z, LRS, 'PolyhedralSurface'(Polygons)) -->
  "(",
  'wkt+'(polygon_text_body(Z, LRS), Polygons),
  ")".

polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface) -->
  "PolyhedralSurface",
  z_m(Z, LRS),
  polyhedralsurface_text(Z, LRS, PolyhedralSurface).



ring_text(Z, LRS, LineString) -->
  linestring_text_body(Z, LRS, LineString), !.
ring_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString), !.
ring_text(Z, LRS, CompoundCurve) -->
  compoundcurve_text_representation(Z, LRS, CompoundCurve).



single_curve_text(Z, LRS, LineString) -->
  linestring_text_body(Z, LRS, LineString), !.
single_curve_text(Z, LRS, CircularString) -->
  circularstring_text_representation(Z, LRS, CircularString).



% CurvePolygon

surface_text(Z, LRS, CurvePolygon) -->
  "CurvePolygon",
  curvepolygon_text_body(Z, LRS, CurvePolygon), !.
surface_text(Z, LRS, Polygon) -->
  polygon_text_body(Z, LRS, Polygon).

surface_text_representation(Z, LRS, CurvePolygon) -->
  curvepolygon_text_representation(Z, LRS, CurvePolygon).



% TIN

tin_text(_, _, 'TIN'([])) --> !,
  "Empty".
tin_text(Z, LRS, 'TIN'([Triangles])) -->
  "(",
  'wkt+'(triangle_text_body(Z, LRS), Triangles),
  ")".

tin_text_representation(Z, LRS, Tin) -->
  "TIN",
  z_m(Z, LRS),
  tin_text(Z, LRS, Tin).



% Triangle

triangle_text(_, _, 'Triangle'([])) --> !,
  "Empty".
triangle_text(Z, LRS, 'Triangle'([LineString])) -->
  "(",
  linestring_text(Z, LRS, LineString),
  ")".

triangle_text_body(Z, LRS, Triangle) -->
  triangle_text(Z, LRS, Triangle).

triangle_text_representation(Z, LRS, Triangle) -->
  "Triangle",
  z_m(Z, LRS),
  triangle_text_body(Z, LRS, Triangle).



%! wkt_generate(+Shape:compound)// is det.

wkt_generate(shape(Z,LRS,Crs,Shape)) -->
  {
    default_value(Z, false),
    default_value(LRS, false),
    default_value(Crs, 'http://www.opengis.net/def/crs/OGC/1.3/CRS84')
  },
  crs(Crs),
  wkt_representation(Z, LRS, Shape).

crs('http://www.opengis.net/def/crs/OGC/1.3/CRS84') --> !, "".
crs(Crs) --> "<", atom(Crs), "> ".

wkt_representation(Z, LRS, Point) -->
  point_text_representation(Z, LRS, Point), !.
wkt_representation(Z, LRS, Curve) -->
  curve_text_representation(Z, LRS, Curve), !.
wkt_representation(Z, LRS, Surface) -->
  surface_text_representation(Z, LRS, Surface), !.
wkt_representation(Z, LRS, Collection) -->
  collection_text_representation(Z, LRS, Collection).





% HELPERS %

%! m(+Number)// is det.

m(N) -->
  number(N).



%! point(+Z:boolean, +LRS:boolean, +Coords:list(number))// is det.

point(false, false, 'Point'([X,Y])) --> !,
  'X'(X),
  " ",
  'Y'(Y).
point(false, true, 'Point'([X,Y,LRS])) --> !,
  point(false, false, 'Point'([X,Y])),
  " ",
  m(LRS).
point(true, false, 'Point'([X,Y,Z])) --> !,
  point(false, false, 'Point'([X,Y])),
  " ",
  'Z'(Z).
point(true, true, 'Point'([X,Y,Z,LRS])) -->
  point(true, false, 'Point'([X,Y,Z])),
  " ",
  m(LRS).



'wkt+'(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H),
  'wkt*'(Dcg_1, T).



'wkt*'(Dcg_1, [H|T]) -->
  ",", !,
  dcg_call(Dcg_1, H),
  'wkt*'(Dcg_1, T).
'wkt*'(_, []) --> "".



%! 'X'(+Number)// is det.

'X'(N) -->
  {must_be(number, N)},
  number(N).



%! 'Y'(+Number)// is det.

'Y'(N) -->
  {must_be(number, N)},
  number(N).



%! 'Z'(+Number)// is det.

'Z'(N) -->
  {must_be(number, N)},
  number(N).



%! z_m(+Z:boolean, +LRS:boolean)// is det.

z_m(false, false) --> !, "".
z_m(false, true) --> !, "M ".
z_m(true, false) --> !, "Z ".
z_m(true, true) --> "ZM ".
