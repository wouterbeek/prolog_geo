:- module(
  gis,
  [
    gis_area/2,            % +Shape, -Area
    gis_is_shape/1,        % @Term
    gis_max/2,             % +Shape, -Maximums
    gis_min/2,             % +Shape, -Minimums
    gis_property/1,        % ?Property
    gis_shape_dimension/2, % +Shape, -Dimension
    gis_shape_type/2,      % +Shape, -Type
    gis_type/1             % ?Type
  ]
).

/** <module> GIS

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(shlib)).

:- use_module(library(call_ext)).

:- use_foreign_library(foreign(gis)).





%! gis_area(+Shape:compound, -Area:float) is det.

gis_area(shape(_,_,_,'Line'(_)), 0.0) :- !.
gis_area(shape(_,_,_,'MultiLine'(_)), 0.0) :- !.
gis_area(shape(_,_,_,'MultiPoint'(_)), 0.0) :- !.
gis_area(shape(_,_,_,'MultiPolygon'(Polygons)), Area) :- !,
  maplist(polygon_area_, Polygons, Areas),
  sum_list(Areas, Area).
gis_area(shape(_,_,_,'Point'(_)), 0.0) :- !.
gis_area(shape(_,_,_,'Polygon'(LineStrings)), Area) :-
  polygon_area_(LineStrings, Area).

polygon_area_([LineString], Area) :- !,
  polygon_area_(LineString, 0.0, Area).
polygon_area_([LineString1,LineString2], Area) :-
  maplist(polygon_area_, [LineString1,LineString2], [Area1,Area2]),
  Area is abs(Area1 - Area2).

polygon_area_([[X1,Y1|_],[X2,Y2|T2]|Coords], Sum1, Area) :- !,
  Sum2 is Sum1 + (X1 * Y2) - (Y1 * X2),
  polygon_area_([[X2,Y2|T2]|Coords], Sum2, Area).
polygon_area_(_, Sum, Area) :-
  Area is abs(Sum / 2.0).



%! gis_is_shape(@Term) is semidet.

gis_is_shape(Shape) :-
  gis_shape_type(Shape, _).



%! gis_max(+Shape:compound, -Maximums:list(float)) is det.

gis_max(shape(_,_,_,Term), Maxs) :-
  gis_shape_dimension_(Term, Dim),
  gis_max_(Dim, Term, Maxs).

gis_max_(2, Term, [X,Y]) :- !,
  gis_max_2(Term, X, Y).
gis_max_(3, Term, [X,Y,Z]) :- !,
  gis_max_3(Term, X, Y, Z).
gis_max_(4, Term, [X,Y,Z,LRS]) :-
  gis_max_4(Term, X, Y, Z, LRS).

gis_max_2('Point'([X,Y]), X, Y) :- !.
gis_max_2(Term, X, Y) :-
  Term =.. [Type,Terms],
  call_must_be(gis_type, Type),
  maplist(gis_max_2, Terms, Xs, Ys),
  maplist(max_list, [Xs,Ys], [X,Y]).

gis_max_3('Point'([X,Y,Z]), X, Y, Z) :- !.
gis_max_3(Term, X, Y, Z) :-
  Term =.. [Type,Terms],
  call_must_be(gis_type, Type),
  maplist(gis_max_3, Terms, Xs, Ys, Zs),
  maplist(max_list, [Xs,Ys,Zs], [X,Y,Z]).

gis_max_4('Point'([X,Y,Z,LRS]), X, Y, Z, LRS) :- !.
gis_max_4(Term, X, Y, Z, LRS) :-
  Term =.. [Type,Terms],
  call_must_be(gis_type, Type),
  maplist(gis_max_4, Terms, Xs, Ys, Zs, LRSs),
  maplist(max_list, [Xs,Ys,Zs,LRSs], [X,Y,Z,LRS]).



%! gis_min(+Shape:compound, -Minimums:list(float)) is det.

gis_min(shape(_,_,_,Term), Mins) :-
  gis_shape_dimension_(Term, Dim),
  gis_min_(Dim, Term, Mins).

gis_min_(2, Term, [X,Y]) :-
  gis_min_2(Term, X, Y).
gis_min_(3, Term, [X,Y,Z]) :-
  gis_min_3(Term, X, Y, Z).
gis_min_(4, Term, [X,Y,Z,LRS]) :-
  gis_min_4(Term, X, Y, Z, LRS).

gis_min_2('Point'([X,Y]), X, Y) :- !.
gis_min_2(Term, X, Y) :-
  Term =.. [Type,Terms],
  call_must_be(gis_type, Type),
  maplist(gis_min_2, Terms, Xs, Ys),
  maplist(min_list, [Xs,Ys], [X,Y]).

gis_min_3('Point'([X,Y,Z]), X, Y, Z) :- !.
gis_min_3(Term, X, Y, Z) :-
  Term =.. [Type,Terms],
  call_must_be(gis_type, Type),
  maplist(gis_min_3, Terms, Xs, Ys, Zs),
  maplist(min_list, [Xs,Ys,Zs], [X,Y,Z]).

gis_min_4('Point'([X,Y,Z,LRS]), X, Y, Z, LRS) :- !.
gis_min_4(Term, X, Y, Z, LRS) :-
  Term =.. [Type,Terms],
  call_must_be(gis_type, Type),
  maplist(gis_min_4, Terms, Xs, Ys, Zs, LRSs),
  maplist(min_list, [Xs,Ys,Zs,LRSs], [X,Y,Z,LRS]).



%! gis_property(?Property:compound) is nondet.

gis_property(Property) :-
  gis_property__(Property),
  gis_property_(Property).

gis_property__(geos_version(_)).



%! gis_shape_dimension(+Shape:compound, -Dimension:positive_integer) is det.

gis_shape_dimension(shape(_,_,_,Term), Dim) :-
  gis_shape_dimension_(Term, Dim).

gis_shape_dimension_('Point'([_]), 1) :- !.
gis_shape_dimension_('Point'([_,_]), 2) :- !.
gis_shape_dimension_('Point'([_,_,_]), 3) :- !.
gis_shape_dimension_('Point'([_,_,_,_]), 4) :- !.
gis_shape_dimension_(Term, Dim) :-
  Term =.. [Type,Shapes],
  call_must_be(gis_type, Type),
  maplist(gis_shape_dimension_, Shapes, [Dim|Dims]),
  (maplist(=(Dim), Dims) -> true ; type_error(gis_dimensions,[Dim|Dims])).



%! gis_shape_type(+Shape:compound, -Type:atom) is det.

gis_shape_type(shape(_,_,_,Term), Type) :-
  Term =.. [Type,_],
  gis_type(Type).



%! gis_type(+Type:atom) is semidet.
%! gis_type(-Type:atom) is multi.

gis_type('CircularString').
gis_type('CompoundCurve').
gis_type('CurvePolygon').
gis_type('GeometryCollection').
gis_type('LineString').
gis_type('MultiCurve').
gis_type('MultiLineString').
gis_type('MultiPoint').
gis_type('MultiPolygon').
gis_type('MultiSurface').
gis_type('Point').
gis_type('Polygon').
gis_type('PolyhedralSurface').
gis_type('TIN').
gis_type('Triangle').
