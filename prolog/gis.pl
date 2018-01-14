:- module(
  gis,
  [
    bearing/3,              % +Point1, +Point2, -Bearing
    box_polygon/2,          % +Box, -Polygon
    convert_distance/3,     % +Distance1, +Kind, -Distance2
    degree_radian/2,        % ?Degree, ?Radian
    distance_greatcircle/3, % +Point1, +Point2, -Distance
    distance_greatcircle/4, % +Point1, +Point2, -Distance, +Unit
    distance_pythagorean/3, % +Point1, +Point2, -Distance
    gis_distance/3,         % +Wkt1, +Wkt2, -Distance
    gis_property/1,         % ?Property
    gis_touches/2,          % +Wkt1, +Wkt2
    gis_union/3,            % +Wkt1, +Wkt2, -Wkt3
    iri_shape/2,            % ?Iri, ?Shape
    is_shape/1,             % +Shape
    shape_dimensionality/2, % +Shape, -Dimensionality
    shape_type/2            % +Wkt, ?Type
  ]
).

/** <module> Gis

@author Wouter Beek
@version 2018/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt_parse)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_db), [rdf_has/3]).
:- use_module(library(shlib)).

:- use_foreign_library(foreign(gis)).

:- rdf_meta
   iri_shape(r, -).





%! bearing(+Point1:compound, +Point2:compound, -Bearing) is det.

bearing(point(Lat1deg,Long1deg), point(Lat2deg,Long2deg), Bearing) :-
  degree_radian(Lat1deg, Lat1),
  degree_radian(Lat2deg, Lat2),
  degree_radian(Long1deg, Long1),
  degree_radian(Long2deg, Long2),
  DLong is Long2 - Long1,
  Y is sin(DLong) * cos(Lat2),
  X is cos(Lat1) * sin(Lat2) - sin(Lat1) * cos(Lat2) * cos(DLong),
  Bearing0 is atan(Y, X),
  degree_radian(Bearing, Bearing0).



%! box_polygon(+Box:compound, -Polygon:compound) is det.

box_polygon(
  box(point(Lx,Ly),point(Hx,Hy)),
  polygon([[point(Lx,Ly),point(Lx,Hy),point(Hx,Hy),point(Hx,Ly),point(Lx,Ly)]])
).



%! convert_distance(+Distance1:float, +Kind:oneof([metre]),
%!                  -Distance2:float) is det.

convert_distance(Distance1, metre, Distance2) :-
  Distance2 is Distance1 * 1 000.



%! degree_radian(+Degree:float, -Radian:float) is det.
%! degree_radian(-Degree:float, +Radian:float) is det.

degree_radian(Degree, Radian) :-
  ground(Degree), !,
  Radian is (Degree * pi) / 180.
degree_radian(Degree, Radian) :-
  ground(Radian), !,
  Degree is (Radian * 180) / pi.
degree_radian(Degree, Radian) :-
  instantiation_error(args([Degree,Radian])).



%! distance_greatcircle(+Point1:compound, +Point2:compound, -Distance:float) is det.
%! distance_greatcircle(+Point1:compound, +Point2:compound, -Distance:float,
%!                      +Unit:oneof([km,nm])) is det.
%
% Calculates great circle distance between Point1 and Point2 in the
% specified Unit, which can take as a value km (kilometers) or nm
% (nautical miles).
%
% @param Unit The unit of measure, either `nm' (the default) for
%             nautic miles or `km' for kilometers.

distance_greatcircle(Point1, Point2, Distance) :-
  distance_greatcircle(Point1, Point2, Distance, nm).


distance_greatcircle(point(Lat1deg,Long1deg), point(Lat2deg,Long2deg), Distance, Unit) :-
  (   Unit == km
  ->  R is 6371
  ;   Unit == nm
  ->  R is 3440.06
  ;   must_be(oneof([km,nm]), Unit)
  ),
  % Haversine formula
  degree_radian(Lat1deg, Lat1),
  degree_radian(Lat2deg, Lat2),
  degree_radian(Long1deg, Long1),
  degree_radian(Long2deg, Long2),
  DLat is Lat2 - Lat1,
  DLong is Long2 - Long1,
  A is (sin(DLat/2)**2) + cos(Lat1) * cos(Lat2) * (sin(DLong/2)**2),
  SqA is sqrt(A),
  OnemA is 1 - A,
  Sq1mA is sqrt(OnemA),
  C is 2 * atan(SqA, Sq1mA),
  Distance is R * C.



%! distance_pythagorean(+Point1:compound, +Point2:compound, -Distance:float) is det.

distance_pythagorean(point(X1,Y1), point(X2,Y2), Distance) :-
  Distance is sqrt(((X2 - X1) ^ 2) + ((Y2 - Y1) ^ 2)).



%! gis_distance(+Wkt1:compound, +Wkt2:compound, -Distance:float) is det.

gis_distance(Wkt1, Wkt2, Distance) :-
  gis_distance_(Wkt1, Wkt2, Distance).



%! gis_property(?Property:compound) is nondet.

gis_property(Property) :-
  gis_property__(Property),
  gis_property_(Property).

gis_property__(geos_version(_)).



%! gis_touches(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_touches(Wkt1, Wkt2) :-
  gis_touches_(Wkt1, Wkt2).



%! gis_union(+Wkt1:atom, +Wkt2:atom, +Wkt3:atom) is det.

gis_union(Wkt1, Wkt2, Wkt3) :-
  gis_union_(Wkt1, Wkt2, Wkt3).



%! iri_shape(?Iri:atom, ?Shape:compound) is nondet.

iri_shape(Iri, Shape) :-
  rdf_has(Iri, geo:hasGeometry, Geometry),
  rdf_has(Geometry, geo:asWKT, literal(type(geo:wktLiteral,Lex))),
  atom_phrase(wkt_parse(Shape), Lex).



%! is_shape(+Shape:compound) is det.
%
% Checks whether Shape is a valid supported shape.

is_shape(Shape) :-
  shape_dimensionality(Shape, _).



%! shape_dimensionality(+Shape:compound, -Dimensionality:nonneg) is det.

shape_dimensionality(box(Point,_), Dim) :- !,
  shape_dimensionality(Point, Dim).
shape_dimensionality('CircularString'(Point,_,_), Dim) :- !,
  shape_dimensionality(Point, Dim).
shape_dimensionality('GeometryCollection'([Geom|_]), Dim) :- !,
  shape_dimensionality(Geom, Dim).
shape_dimensionality('LineString'([Point|_]), Dim) :- !,
  shape_dimensionality(Point, Dim).
shape_dimensionality('MultiLineString'([LineString|_]), Dim) :- !,
  shape_dimensionality(LineString, Dim).
shape_dimensionality('MultiPoint'([Point|_]), Dim) :- !,
  shape_dimensionality(Point, Dim).
shape_dimensionality('MultiPolygon'([Polygon|_]), Dim) :- !,
  shape_dimensionality(Polygon, Dim).
shape_dimensionality('Polygon'([[Point|_]|_]), Dim) :- !,
  shape_dimensionality(Point, Dim).
shape_dimensionality(Shape, Dim) :-
  ground(Shape), !,
  functor(Shape, 'Point', Dim).
shape_dimensionality(Shape, _) :-
  type_error(shape, Shape).



%! shape_type(+Wkt:atom, +Type:atom) is semidet.
%! shape_type(+Wkt:atom, -Type:atom) is det.

shape_type(Wkt, Type) :-
  shape_type_(Wkt, Type).
