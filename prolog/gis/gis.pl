:- module(
  gis,
  [
    gis_boundary/2,             % +Wkt, -Boundary
    gis_contains/2,             % +Wkt1, +Wkt2
    gis_convex_hull/2,          % +Wkt, -ConvexHull
    gis_crosses/2,              % +Wkt1, +Wkt2
    gis_difference/3,           % +Wkt1, +Wkt2, -Difference
    gis_disjoint/2,             % +Wkt1, +Wkt2
    gis_distance/3,             % +Wkt1, +Wkt2, -Distance
    gis_envelope/2,             % +Wkt, -Envelope
    gis_equals/2,               % +Wkt1, +Wkt2
    gis_intersection/3,         % +Wkt1, +Wkt2, -Intersection
    gis_intersects/2,           % +Wkt1, +Wkt2
    gis_overlaps/2,             % +Wkt1, +Wkt2
    gis_property/1,             % ?Property
    gis_symmetric_difference/3, % +Wkt1, +Wkt2, -Difference
    gis_touches/2,              % +Wkt1, +Wkt2
    gis_type/2,                 % +Wkt, ?Type
    gis_union/3,                % +Wkt1, +Wkt2, -Wkt3
    gis_within/2,               % +Wkt1, +Wkt2
    is_shape/1,                 % +Shape
    shape_dimensionality/2,     % +Shape, -Dimensionality
    shape_type/1,               % ?Type
    shape_type/2                % +Shape, -Type
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
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).

:- use_foreign_library(foreign(gis)).

:- rdf_assert_prefix(geo, 'http://www.opengis.net/ont/geosparql#').





%! gis_boundary(+Wkt:atom, -Boundary:atom) is det.

gis_boundary(Wkt, Boundary) :-
  gis_boundary_(Wkt, Boundary).



%! gis_contains(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_contains(Wkt1, Wkt2) :-
  gis_contains_(Wkt1, Wkt2).



%! gis_convex_hull(+Wkt:atom, -ConvexHull:atom) is det.

gis_convex_hull(Wkt, ConvexHull) :-
  gis_convex_hull_(Wkt, ConvexHull).



%! gis_crosses(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_crosses(Wkt1, Wkt2) :-
  gis_crosses_(Wkt1, Wkt2).



%! gis_difference(+Wkt1:atom, +Wkt2:atom, -Difference:atom) is det.

gis_difference(Wkt1, Wkt2, Difference) :-
  gis_difference_(Wkt1, Wkt2, Difference).



%! gis_disjoint(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_disjoint(Wkt1, Wkt2) :-
  gis_disjoint_(Wkt1, Wkt2).



%! gis_distance(+Wkt1:atom, +Wkt2:atom, -Distance:double) is det.

gis_distance(Wkt1, Wkt2, Distance) :-
  gis_distance_(Wkt1, Wkt2, Distance).



%! gis_envelope(+Wkt:atom, -Envelope:atom) is det.

gis_envelope(Wkt, Envelope) :-
  gis_envelope_(Wkt, Envelope).



%! gis_equals(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_equals(Wkt1, Wkt2) :-
  gis_equals_(Wkt1, Wkt2).



%! gis_intersection(+Wkt1:atom, +Wkt2:atom, -Intersection:atom) is det.

gis_intersection(Wkt1, Wkt2, Intersection) :-
  gis_intersection_(Wkt1, Wkt2, Intersection).



%! gis_intersects(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_intersects(Wkt1, Wkt2) :-
  gis_intersects_(Wkt1, Wkt2).



%! gis_overlaps(+Wkt1:atom, +Wkt2:atom) is semidet.

gis_overlaps(Wkt1, Wkt2) :-
  gis_overlaps_(Wkt1, Wkt2).



%! gis_property(?Property:compound) is nondet.

gis_property(Property) :-
  gis_property__(Property),
  gis_property_(Property).

gis_property__(geos_version(_)).



%! gis_symmetric_difference(+Wkt1:atom, +Wkt2:atom, -Difference:atom) is det.

gis_symmetric_difference(Wkt1, Wkt2, Difference) :-
  gis_symmetric_difference_(Wkt1, Wkt2, Difference).



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
