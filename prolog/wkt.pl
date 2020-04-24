:- module(
  wkt,
  [
    wkt_boundary/2,             % +Wkt, -Boundary
    wkt_contains/2,             % +Wkt1, +Wkt2
    wkt_convex_hull/2,          % +Wkt, -ConvexHull
    wkt_crosses/2,              % +Wkt1, +Wkt2
    wkt_difference/3,           % +Wkt1, +Wkt2, -Difference
    wkt_disjoint/2,             % +Wkt1, +Wkt2
    wkt_distance/3,             % +Wkt1, +Wkt2, -Distance
    wkt_envelope/2,             % +Wkt, -Envelope
    wkt_equals/2,               % +Wkt1, +Wkt2
    wkt_intersection/3,         % +Wkt1, +Wkt2, -Intersection
    wkt_intersects/2,           % +Wkt1, +Wkt2
    wkt_overlaps/2,             % +Wkt1, +Wkt2
    wkt_shape_atom/2,           % ?Shape, ?Atom
    wkt_symmetric_difference/3, % +Wkt1, +Wkt2, -Difference
    wkt_touches/2,              % +Wkt1, +Wkt2
    wkt_union/3,                % +Wkt1, +Wkt2, -Wkt3
    wkt_within/2                % +Wkt1, +Wkt2
  ]
).

/** <module> Well-Known Text (WKT) support

*/

:- use_module(library(error)).
:- use_module(library(shlib)).

:- use_module(library(dcg)).
:- use_module(library(wkt_generate)).
:- use_module(library(wkt_parse)).

:- use_foreign_library(foreign(geo)).





%! wkt_boundary(+Wkt:atom, -Boundary:atom) is det.

wkt_boundary(Wkt, Boundary) :-
  wkt_boundary_(Wkt, Boundary).



%! wkt_contains(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_contains(Wkt1, Wkt2) :-
  wkt_contains_(Wkt1, Wkt2).



%! wkt_convex_hull(+Wkt:atom, -ConvexHull:atom) is det.

wkt_convex_hull(Wkt, ConvexHull) :-
  wkt_convex_hull_(Wkt, ConvexHull).



%! wkt_crosses(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_crosses(Wkt1, Wkt2) :-
  wkt_crosses_(Wkt1, Wkt2).



%! wkt_difference(+Wkt1:atom, +Wkt2:atom, -Difference:atom) is det.

wkt_difference(Wkt1, Wkt2, Difference) :-
  wkt_difference_(Wkt1, Wkt2, Difference).



%! wkt_disjoint(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_disjoint(Wkt1, Wkt2) :-
  wkt_disjoint_(Wkt1, Wkt2).



%! wkt_distance(+Wkt1:atom, +Wkt2:atom, -Distance:double) is det.

wkt_distance(Wkt1, Wkt2, Distance) :-
  wkt_distance_(Wkt1, Wkt2, Distance).



%! wkt_envelope(+Wkt:atom, -Envelope:atom) is det.

wkt_envelope(Wkt, Envelope) :-
  wkt_envelope_(Wkt, Envelope).



%! wkt_equals(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_equals(Wkt1, Wkt2) :-
  wkt_equals_(Wkt1, Wkt2).



%! wkt_intersection(+Wkt1:atom, +Wkt2:atom, -Intersection:atom) is det.

wkt_intersection(Wkt1, Wkt2, Intersection) :-
  wkt_intersection_(Wkt1, Wkt2, Intersection).



%! wkt_intersects(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_intersects(Wkt1, Wkt2) :-
  wkt_intersects_(Wkt1, Wkt2).



%! wkt_overlaps(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_overlaps(Wkt1, Wkt2) :-
  wkt_overlaps_(Wkt1, Wkt2).



%! wkt_shape_atom(+Shape:compound, -Atom:atom)

wkt_shape_atom(Shape, Atom) :-
  nonvar(Shape), !,
  atom_phrase(wkt_generate(Shape), Atom).
wkt_shape_atom(Shape, Atom) :-
  nonvar(Atom), !,
  atom_phrase(wkt_parse(Shape), Atom).
wkt_shape_atom(Shape, Atom) :-
  instantiation_error(args([Shape,Atom])).



%! wkt_symmetric_difference(+Wkt1:atom, +Wkt2:atom, -Difference:atom) is det.

wkt_symmetric_difference(Wkt1, Wkt2, Difference) :-
  wkt_symmetric_difference_(Wkt1, Wkt2, Difference).



%! wkt_touches(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_touches(Wkt1, Wkt2) :-
  wkt_touches_(Wkt1, Wkt2).



%! wkt_union(+Wkt1:atom, +Wkt2:atom, +Wkt3:atom) is det.

wkt_union(Wkt1, Wkt2, Wkt3) :-
  wkt_union_(Wkt1, Wkt2, Wkt3).



%! wkt_within(+Wkt1:atom, +Wkt2:atom) is semidet.

wkt_within(Wkt1, Wkt2) :-
  wkt_within_(Wkt1, Wkt2).
