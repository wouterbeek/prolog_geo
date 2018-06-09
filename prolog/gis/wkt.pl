:- module(
  wkt,
  [
    wkt_shape_dimension/2, % +Shape, -Dimension
    wkt_is_shape/1,        % @Term
    wkt_max/2,             % +Shape, -Maximums
    wkt_min/2,             % +Shape, -Minimums
    wkt_shape_atom/2,      % ?Shape, ?Atom
    wkt_type/1,            % ?Type
    wkt_shape_type/2       % +Shape, -Type
  ]
).

/** <module> Well-Known Text (WKT)

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(error)).
:- use_module(library(gis/wkt_generate)).
:- use_module(library(gis/wkt_parser)).
:- use_module(library(lists)).





%! wkt_shape_dimension(+Shape:compound, -Dimensionality:nonneg) is det.

wkt_shape_dimension('Point'([_]), 1) :- !.
wkt_shape_dimension('Point'([_,_]), 2) :- !.
wkt_shape_dimension('Point'([_,_,_]), 3) :- !.
wkt_shape_dimension('Point'([_,_,_,_]), 4) :- !.
wkt_shape_dimension(Shape, Dim) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_shape_dimension, Shapes, [Dim|Dims]),
  (maplist(=(Dim), Dims) -> true ; type_error(wkt_dimensions,[Dim|Dims])).



%! wkt_is_shape(@Term) is semidet.

wkt_is_shape(Term) :-
  wkt_shape_type(Term, _).



%! wkt_max(+Shape:compound, -Maximums:list(float)) is det.

wkt_max(Shape, Maxs) :-
  wkt_shape_dimension(Shape, Dim),
  wkt_max_(Dim, Shape, Maxs).

wkt_max_(2, Shape, [X,Y]) :-
  wkt_max_2(Shape, X, Y).
wkt_max_(3, Shape, [X,Y,Z]) :-
  wkt_max_3(Shape, X, Y, Z).
wkt_max_(4, Shape, [X,Y,Z,LRS]) :-
  wkt_max_4(Shape, X, Y, Z, LRS).

wkt_max_2('Point'([X,Y]), X, Y) :- !.
wkt_max_2(Shape, X, Y) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_max_2, Shapes, Xs, Ys),
  maplist(max_list, [Xs,Ys], [X,Y]).

wkt_max_3('Point'([X,Y,Z]), X, Y, Z) :- !.
wkt_max_3(Shape, X, Y, Z) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_max_3, Shapes, Xs, Ys, Zs),
  maplist(max_list, [Xs,Ys,Zs], [X,Y,Z]).

wkt_max_4('Point'([X,Y,Z,LRS]), X, Y, Z, LRS) :- !.
wkt_max_4(Shape, X, Y, Z, LRS) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_max_4, Shapes, Xs, Ys, Zs, LRSs),
  maplist(max_list, [Xs,Ys,Zs,LRSs], [X,Y,Z,LRS]).



%! wkt_min(+Shape:compound, -Minimums:list(float)) is det.

wkt_min(Shape, Mins) :-
  wkt_shape_dimension(Shape, Dim),
  wkt_min_(Dim, Shape, Mins).

wkt_min_(2, Shape, [X,Y]) :-
  wkt_min_2(Shape, X, Y).
wkt_min_(3, Shape, [X,Y,Z]) :-
  wkt_min_3(Shape, X, Y, Z).
wkt_min_(4, Shape, [X,Y,Z,LRS]) :-
  wkt_min_4(Shape, X, Y, Z, LRS).

wkt_min_2('Point'([X,Y]), X, Y) :- !.
wkt_min_2(Shape, X, Y) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_min_2, Shapes, Xs, Ys),
  maplist(min_list, [Xs,Ys], [X,Y]).

wkt_min_3('Point'([X,Y,Z]), X, Y, Z) :- !.
wkt_min_3(Shape, X, Y, Z) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_min_3, Shapes, Xs, Ys, Zs),
  maplist(min_list, [Xs,Ys,Zs], [X,Y,Z]).

wkt_min_4('Point'([X,Y,Z,LRS]), X, Y, Z, LRS) :- !.
wkt_min_4(Shape, X, Y, Z, LRS) :-
  Shape =.. [Type,Shapes],
  wkt_type_check(Type),
  maplist(wkt_min_4, Shapes, Xs, Ys, Zs, LRSs),
  maplist(min_list, [Xs,Ys,Zs,LRSs], [X,Y,Z,LRS]).



%! wkt_shape_atom(+Shape:compound, -Atom:atom)

wkt_shape_atom(Shape, Atom) :-
  ground(Shape), !,
  atom_phrase(wkt_generate(Shape), Atom).
wkt_shape_atom(Shape, Atom) :-
  ground(Atom), !,
  atom_phrase(wkt_parse(Shape), Atom).
wkt_shape_atom(Shape, Atom) :-
  instantiation_error(args([Shape,Atom])).



%! wkt_shape_type(+Shape:compound, -Type:atom) is det.

wkt_shape_type(Shape, Type) :-
  Shape =.. [Type,_],
  wkt_type_check(Type).



%! wkt_type(+Type:atom) is semidet.
%! wkt_type(-Type:atom) is multi.

wkt_type('CircularString').
wkt_type('CompoundCurve').
wkt_type('CurvePolygon').
wkt_type('GeometryCollection').
wkt_type('LineString').
wkt_type('MultiCurve').
wkt_type('MultiLineString').
wkt_type('MultiPoint').
wkt_type('MultiPolygon').
wkt_type('MultiSurface').
wkt_type('Point').
wkt_type('Polygon').
wkt_type('PolyhedralSurface').
wkt_type('TIN').
wkt_type('Triangle').





% HELPERS %

wkt_type_check(Type) :-
  wkt_type(Type), !.
wkt_type_check(Type) :-
  existence_error(wkt_type, Type).
