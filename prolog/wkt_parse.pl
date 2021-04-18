:- module(
  wkt_parse,
  [
    wkt_parse//1 % -Shape:compound
  ]
).

/** <module> Well-Known Text (WKT) parser support

```pl
shape(Z:boolean, LRS:boolean, Crs:iri, Shape:compound)
```

*/

:- use_module(library(abnf)).
:- use_module(library(dcg)).



%! wkt_parse(-Shape:compound)// is det.

wkt_parse(shape(Z,LRS,Crs,Shape)) -->
  (   "<",
      ...(Codes),
      ">"
  ->  blank,
      blanks, !,
      {atom_codes(Crs, Codes)}
  ;   {Crs = 'http://www.opengis.net/def/crs/OGC/1.3/CRS84'}
  ),
  wkt_representation(Z, LRS, Shape).





% GRAMMAR %

% CircularString

circularstring_text(Z, LRS, 'CircularString'(Coords)) -->
  (   "("
  ->  '+&!'(coord(Z, LRS), ",", Coords),
      must_see_code(0'))
  ;   empty(Coords)
  ).

circularstring_text_representation(Z, LRS, CircularString) -->
  keyword(`circularstring`),
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

compoundcurve_text(Z, LRS, 'CompoundCurve'(Curves)) -->
  (   "("
  ->  '+&!'(single_curve_text(Z, LRS), ",", Curves),
      must_see_code(0'))
  ;   empty(Curves)
  ).

compoundcurve_text_representation(Z, LRS, CompoundCurve) -->
  keyword(`compoundcurve`),
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

curvepolygon_text(Z, LRS, 'CurvePolygon'(Rings)) -->
  (   "("
  ->  '+&!'(ring_text(Z, LRS), ",", Rings),
      must_see_code(0'))
  ;   empty(Rings)
  ).

curvepolygon_text_body(Z, LRS, CurvePolygon) -->
  curvepolygon_text(Z, LRS, CurvePolygon).

curvepolygon_text_representation(Z, LRS, CurvePolygon) -->
  keyword(`curvepolygon`),
  z_m(Z, LRS),
  curvepolygon_text_body(Z, LRS, CurvePolygon), !.
curvepolygon_text_representation(Z, LRS, Polygon) -->
  polygon_text_representation(Z, LRS, Polygon), !.
curvepolygon_text_representation(Z, LRS, Triangle) -->
  triangle_text_representation(Z, LRS, Triangle).



% GeometryCollection

geometrycollection_text(Z, LRS, 'GeometryCollection'(Shapes)) -->
  (   "("
  ->  '+&!'(wkt_representation(Z, LRS), ",", Shapes),
      must_see_code(0'))
  ;   empty(Shapes)
  ).

geometrycollection_text_representation(Z, LRS, GeometryCollection) -->
  keyword(`geometrycollection`),
  z_m(Z, LRS),
  geometrycollection_text(Z, LRS, GeometryCollection).



% LineString

linestring_text(Z, LRS, 'LineString'(Coords)) -->
  (   "("
  ->  '+&!'(coord(Z, LRS), ",", Coords),
      must_see_code(0'))
  ;   empty(Coords)
  ).

linestring_text_body(Z, LRS, LineString) -->
  linestring_text(Z, LRS, LineString).

linestring_text_representation(Z, LRS, LineString) -->
  keyword(`linestring`),
  z_m(Z, LRS),
  linestring_text_body(Z, LRS, LineString).



% MultiCurve

multicurve_text(Z, LRS, 'MultiCurve'(Curves)) -->
  (   "("
  ->  '+&!'(curve_text(Z, LRS), ",", Curves),
      must_see_code(0'))
  ;   empty(Curves)
  ).

multicurve_text_representation(Z, LRS, MultiCurve) -->
  keyword(`multicurve`),
  z_m(Z, LRS),
  multicurve_text(Z, LRS, MultiCurve), !.
multicurve_text_representation(Z, LRS, MultiLineString) -->
  multilinestring_text_representation(Z, LRS, MultiLineString).



% MultiLineString

multilinestring_text(Z, LRS, 'MultiLineString'(LineStrings)) -->
  (   "("
  ->  '+&!'(linestring_text_body(Z, LRS), ",", LineStrings),
      must_see_code(0'))
  ;   empty(LineStrings)
  ).

multilinestring_text_representation(Z, LRS, MultiLineString) -->
  keyword(`multilinestring`),
  z_m(Z, LRS),
  multilinestring_text(Z, LRS, MultiLineString).



% MultiPoint

multipoint_text(Z, LRS, 'MultiPoint'(Coords)) -->
  (   "("
  ->  '+&!'(point_text(Z, LRS), ",", Coords),
      must_see_code(0'))
  ;   empty(Coords)
  ).

multipoint_text_representation(Z, LRS, MultiPoint) -->
  keyword(`multipoint`),
  z_m(Z, LRS),
  multipoint_text(Z, LRS, MultiPoint).



% MultiPolygon

multipolygon_text(Z, LRS, 'MultiPolygon'(Polygons)) -->
  (   "("
  ->  '+&!'(polygon_text_body(Z, LRS), ",", Polygons),
      must_see_code(0'))
  ;   empty(Polygons)
  ).

multipolygon_text_representation(Z, LRS, MultiPolygon) -->
  keyword(`multipolygon`),
  z_m(Z, LRS),
  multipolygon_text(Z, LRS, MultiPolygon).



% MultiSurface

multisurface_text(Z, LRS, 'MultiSurface'(Surfaces)) -->
  (   "("
  ->  '+&!'(surface_text(Z, LRS), ",", Surfaces),
      must_see_code(0'))
  ;   empty(Surfaces)
  ).

multisurface_text_representation(Z, LRS, MultiSurface) -->
  keyword(`multisurface`),
  z_m(Z, LRS),
  multisurface_text(Z, LRS, MultiSurface), !.
multisurface_text_representation(Z, LRS, MultiPolygon) -->
  multipolygon_text_representation(Z, LRS, MultiPolygon), !.
multisurface_text_representation(Z, LRS, PolyhedralSurface) -->
  polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface), !.
multisurface_text_representation(Z, LRS, Tin) -->
  tin_text_representation(Z, LRS, Tin).



% Point

point_text(Z, LRS, Coord) -->
  "(",
  coord(Z, LRS, Coord),
  must_see_code(0')).

point_text_representation(Z, LRS, 'Point'(Coord)) -->
  keyword(`point`),
  z_m(Z, LRS),
  point_text(Z, LRS, Coord).



% Polygon

polygon_text(Z, LRS, 'Polygon'(LineStrings)) -->
  (   "("
  ->  '+&!'(linestring_text(Z, LRS), ",", LineStrings),
      must_see_code(0'))
  ;   empty(LineStrings)
  ).

polygon_text_body(Z, LRS, Polygon) -->
  polygon_text(Z, LRS, Polygon).

polygon_text_representation(Z, LRS, Polygon) -->
  keyword(`polygon`),
  z_m(Z, LRS),
  polygon_text_body(Z, LRS, Polygon).



% PolyhedralSurface

polyhedralsurface_text(Z, LRS, 'PolyhedralSurface'(Polygons)) -->
  (   "("
  ->  '+&!'(polygon_text_body(Z, LRS), ",", Polygons),
      must_see_code(0'))
  ;   empty(Polygons)
  ).

polyhedralsurface_text_representation(Z, LRS, PolyhedralSurface) -->
  keyword(`polyhedralsurface`),
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



surface_text(Z, LRS, CurvePolygon) -->
  keyword(`curvepolygon`),
  curvepolygon_text_body(Z, LRS, CurvePolygon), !.
surface_text(Z, LRS, Polygon) -->
  polygon_text_body(Z, LRS, Polygon).

surface_text_representation(Z, LRS, CurvePolygon) -->
  curvepolygon_text_representation(Z, LRS, CurvePolygon).



% TIN

tin_text(Z, LRS, 'TIN'(Triangles)) -->
  (   "("
  ->  '+&!'(triangle_text_body(Z, LRS), ",", Triangles),
      must_see_code(0'))
  ;   empty(Triangles)
  ).

tin_text_representation(Z, LRS, Tin) -->
  keyword(`tin`),
  z_m(Z, LRS),
  tin_text(Z, LRS, Tin).



% Triangle

triangle_text(Z, LRS, 'Triangle'(LineStrings)) -->
  (   "("
  ->  linestring_text(Z, LRS, LineString),
      {LineStrings = [LineString]},
      must_see_code(0'))
  ;   empty(LineStrings)
  ).

triangle_text_body(Z, LRS, Triangle) -->
  triangle_text(Z, LRS, Triangle).

triangle_text_representation(Z, LRS, Triangle) -->
  keyword(`triangle`),
  z_m(Z, LRS),
  triangle_text_body(Z, LRS, Triangle).



wkt_representation(Z, LRS, Point) -->
  point_text_representation(Z, LRS, Point), !.
wkt_representation(Z, LRS, Curve) -->
  curve_text_representation(Z, LRS, Curve), !.
wkt_representation(Z, LRS, Surface) -->
  surface_text_representation(Z, LRS, Surface), !.
wkt_representation(Z, LRS, Collection) -->
  collection_text_representation(Z, LRS, Collection).





% HELPERS %

%! coord(+Z:boolean, +LRS:boolean, -Coord:compound)// is det.

coord(true, true, coord(X,Y,Z,LRS)) --> !,
  coord(true, false, coord(X,Y,Z)),
  must_see_code(0' ),
  blanks,
  m(LRS).
coord(true, false, coord(X,Y,Z)) --> !,
  coord(false, false, coord(X,Y)),
  must_see_code(0' ),
  blanks,
  'Z'(Z).
coord(false, true, coord(X,Y,LRS)) --> !,
  coord(false, false, coord(X,Y)),
  must_see_code(0' ),
  blanks,
  m(LRS).
coord(false, false, coord(X,Y)) -->
  'X'(X),
  must_see_code(0' ),
  blanks,
  'Y'(Y).



%! empty(-Shapes:list:compound)// .

empty([]) -->
  keyword(`empty`).



%! keyword(+Cs)// .

keyword([H|T]) -->
  alpha(C),
  {code_type(H, to_lower(C))},
  keyword(T).
keyword([]) -->
  (alpha(_) -> !, {fail} ; ""),
  blanks.



%! m(-N)// .

m(N) -->
  number(N).



%! must_see_code(-Code)// .

must_see_code(C) -->
  must_see_code(C, blanks).



%! 'X'(-N)// .

'X'(N) -->
  number(N).



%! 'Y'(-N)// .

'Y'(N) -->
  number(N).



%! 'Z'(-N)// .

'Z'(N) -->
  number(N).



%! z_m(-Z:boolean, -LRS:boolean)// is det.

z_m(Z, LRS) -->
  ("Z" -> blanks, {Z = true} ; {Z = false}),
  ("M" -> blanks, {LRS = true} ; {LRS = false}).
