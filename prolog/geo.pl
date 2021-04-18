:- module(
  geo,
  [
    geo_area/2,            % +Shape, -Area
    geo_is_shape/1,        % @Term
    geo_max/2,             % +Shape, -Maximums
    geo_min/2,             % +Shape, -Minimums
    geo_property/1,        % ?Property
    geo_shape_dimension/2, % +Shape, -Dimension
    geo_shape_type/2,      % +Shape, -Type
    geo_translate_coord/3, % +Crs, +Coord1, -Coord2
    geo_type/1             % ?Type
  ]
).

/** <module> Geospatial predicates

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(shlib)).

:- use_module(library(call_ext)).

:- use_foreign_library(foreign(geo)).



%! geo_area(+Shape:compound, -Area:float) is det.

geo_area(shape(_,_,_,'Line'(_)), 0.0) :- !.
geo_area(shape(_,_,_,'MultiLine'(_)), 0.0) :- !.
geo_area(shape(_,_,_,'MultiPoint'(_)), 0.0) :- !.
geo_area(shape(_,_,_,'MultiPolygon'(Polygons)), Area) :- !,
  maplist(polygon_area_, Polygons, Areas),
  sum_list(Areas, Area).
geo_area(shape(_,_,_,'Point'(_)), 0.0) :- !.
geo_area(shape(_,_,_,'Polygon'(LineStrings)), Area) :-
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



%! geo_is_shape(@Term) is semidet.

geo_is_shape(Shape) :-
  geo_shape_type(Shape, _).



%! geo_max(+Shape:compound, -Maximums:list(float)) is det.

geo_max(shape(_,_,_,Term), Maxs) :-
  geo_shape_dimension_(Term, Dim),
  geo_max_(Dim, Term, Maxs).

geo_max_(2, Term, [X,Y]) :- !,
  geo_max_2(Term, X, Y).
geo_max_(3, Term, [X,Y,Z]) :- !,
  geo_max_3(Term, X, Y, Z).
geo_max_(4, Term, [X,Y,Z,LRS]) :-
  geo_max_4(Term, X, Y, Z, LRS).

geo_max_2('Point'([X,Y]), X, Y) :- !.
geo_max_2(Term, X, Y) :-
  Term =.. [Type,Terms],
  call_must_be(geo_type, Type),
  maplist(geo_max_2, Terms, Xs, Ys),
  maplist(max_list, [Xs,Ys], [X,Y]).

geo_max_3('Point'([X,Y,Z]), X, Y, Z) :- !.
geo_max_3(Term, X, Y, Z) :-
  Term =.. [Type,Terms],
  call_must_be(geo_type, Type),
  maplist(geo_max_3, Terms, Xs, Ys, Zs),
  maplist(max_list, [Xs,Ys,Zs], [X,Y,Z]).

geo_max_4('Point'([X,Y,Z,LRS]), X, Y, Z, LRS) :- !.
geo_max_4(Term, X, Y, Z, LRS) :-
  Term =.. [Type,Terms],
  call_must_be(geo_type, Type),
  maplist(geo_max_4, Terms, Xs, Ys, Zs, LRSs),
  maplist(max_list, [Xs,Ys,Zs,LRSs], [X,Y,Z,LRS]).



%! geo_min(+Shape:compound, -Minimums:list(float)) is det.

geo_min(shape(_,_,_,Term), Mins) :-
  geo_shape_dimension_(Term, Dim),
  geo_min_(Dim, Term, Mins).

geo_min_(2, Term, [X,Y]) :-
  geo_min_2(Term, X, Y).
geo_min_(3, Term, [X,Y,Z]) :-
  geo_min_3(Term, X, Y, Z).
geo_min_(4, Term, [X,Y,Z,LRS]) :-
  geo_min_4(Term, X, Y, Z, LRS).

geo_min_2('Point'([X,Y]), X, Y) :- !.
geo_min_2(Term, X, Y) :-
  Term =.. [Type,Terms],
  call_must_be(geo_type, Type),
  maplist(geo_min_2, Terms, Xs, Ys),
  maplist(min_list, [Xs,Ys], [X,Y]).

geo_min_3('Point'([X,Y,Z]), X, Y, Z) :- !.
geo_min_3(Term, X, Y, Z) :-
  Term =.. [Type,Terms],
  call_must_be(geo_type, Type),
  maplist(geo_min_3, Terms, Xs, Ys, Zs),
  maplist(min_list, [Xs,Ys,Zs], [X,Y,Z]).

geo_min_4('Point'([X,Y,Z,LRS]), X, Y, Z, LRS) :- !.
geo_min_4(Term, X, Y, Z, LRS) :-
  Term =.. [Type,Terms],
  call_must_be(geo_type, Type),
  maplist(geo_min_4, Terms, Xs, Ys, Zs, LRSs),
  maplist(min_list, [Xs,Ys,Zs,LRSs], [X,Y,Z,LRS]).



%! geo_property(?Property:compound) is nondet.

geo_property(Property) :-
  geo_property__(Property),
  geo_property_(Property).

geo_property__(geos_version(_)).



%! geo_shape_dimension(+Shape:compound, -Dimension:positive_integer) is det.

geo_shape_dimension(shape(_,_,_,Term), Dim) :-
  geo_shape_dimension_(Term, Dim).

geo_shape_dimension_('Point'([_]), 1) :- !.
geo_shape_dimension_('Point'([_,_]), 2) :- !.
geo_shape_dimension_('Point'([_,_,_]), 3) :- !.
geo_shape_dimension_('Point'([_,_,_,_]), 4) :- !.
geo_shape_dimension_(Term, Dim) :-
  Term =.. [Type,Shapes],
  call_must_be(geo_type, Type),
  maplist(geo_shape_dimension_, Shapes, [Dim|Dims]),
  (maplist(=(Dim), Dims) -> true ; type_error(geo_dimensions,[Dim|Dims])).



%! geo_shape_type(+Shape:compound, -Type:atom) is det.

geo_shape_type(shape(_,_,_,Term), Type) :-
  Term =.. [Type,_],
  geo_type(Type).



geo_translate_coord(Crs, Coord1, Coord2) :-
  proj_crs_(Crs, Crs0),
  geo_translate_coord_(Crs0, Coord1, Coord2).

proj_crs_('http://www.opengis.net/def/crs/EPSG/0/28992', 'EPSG:28992').
proj_crs_('http://www.opengis.net/def/crs/EPSG/0/4326', 'EPSG:4326').



%! geo_type(+Type:atom) is semidet.
%! geo_type(-Type:atom) is multi.

geo_type('CircularString').
geo_type('CompoundCurve').
geo_type('CurvePolygon').
geo_type('GeometryCollection').
geo_type('LineString').
geo_type('MultiCurve').
geo_type('MultiLineString').
geo_type('MultiPoint').
geo_type('MultiPolygon').
geo_type('MultiSurface').
geo_type('Point').
geo_type('Polygon').
geo_type('PolyhedralSurface').
geo_type('TIN').
geo_type('Triangle').
