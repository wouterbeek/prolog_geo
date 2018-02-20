:- module(
  gis,
  [
    geometry_shape/2,       % ?Geometry, ?Shape
    gis_contains/2,         % +Wkt1, +Wkt2
    gis_distance/3,         % +Wkt1, +Wkt2, -Distance
    gis_intersects/2,       % +Wkt1, +Wkt2
    gis_property/1,         % ?Property
    gis_touches/2,          % +Wkt1, +Wkt2
    gis_type/2,             % +Wkt, ?Type
    gis_union/3,            % +Wkt1, +Wkt2, -Wkt3
    gis_within/2,           % +Wkt1, +Wkt2
    is_shape/1,             % +Shape
    literal_shape/2,        % +Literal, -Shape
    shape_dimensionality/2, % +Shape, -Dimensionality
    shape_type/1,           % ?Type
    shape_type/2            % +Shape, -Type
  ]
).

/** <module> GIS

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).
:- use_module(library(semweb/rdf_db), [
     rdf_has/3
   ]).
:- use_module(library(shlib)).

:- use_module(library(dcg/dcg)).
:- use_module(library(gis/wkt_parse)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).

:- use_foreign_library(foreign(gis)).

:- rdf_assert_prefix(geo, 'http://www.opengis.net/ont/geosparql#').

:- rdf_meta
   geometry_shape(r, -),
   literal_shape(o, -).





%! geometry_shape(?Geometry:rdf_nonliteral, ?Shape:compound) is nondet.

geometry_shape(Geometry, Shape) :-
  rdf_has(Geometry, geo:asWKT, Literal),
  literal_shape(Literal, Shape).



%! gis_contains(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_contains(Wkt1, Wkt2) :-
  gis_contains_(Wkt1, Wkt2).



%! gis_distance(+Wkt1:compound, +Wkt2:compound, -Distance:float) is det.

gis_distance(Wkt1, Wkt2, Distance) :-
  gis_distance_(Wkt1, Wkt2, Distance).



%! gis_property(?Property:compound) is nondet.

gis_property(Property) :-
  gis_property__(Property),
  gis_property_(Property).

gis_property__(geos_version(_)).



%! gis_intersects(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_intersects(Wkt1, Wkt2) :-
  gis_intersects_(Wkt1, Wkt2).



%! gis_touches(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_touches(Wkt1, Wkt2) :-
  gis_touches_(Wkt1, Wkt2).



%! gis_type(+Wkt:atom, +Type:atom) is semidet.
%! gis_type(+Wkt:atom, -Type:atom) is det.

gis_type(Wkt, Type) :-
  shape_type_(Wkt, Type).



%! gis_union(+Wkt1:atom, +Wkt2:atom, +Wkt3:atom) is det.

gis_union(Wkt1, Wkt2, Wkt3) :-
  gis_union_(Wkt1, Wkt2, Wkt3).



%! gis_within(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_within(Wkt1, Wkt2) :-
  gis_within_(Wkt1, Wkt2).



%! is_shape(+Shape:compound) is det.
%
% Checks whether Shape is a valid supported shape.

is_shape(Shape) :-
  shape_dimensionality(Shape, _).
