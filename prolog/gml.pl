:- module(
  gml,
  [
    gml_shape/2 % +Dom, -Shape
  ]
).

/** <module> GML support

*/

:- use_module(library(apply)).

:- use_module(library(dcg)).





%! gml_shape(+Dom, -Shape) is det.
%
% @tbd set Z
% @tbd set LRS
% @tbd set CRS

gml_shape(Dom, shape(_Z,_LRS,_Crs,Shape)) :-
  gml_shape_(Dom, Shape).

gml_shape_(element('gml:coordinates',_,[Coords]), Line) :- !,
  atom_phrase(gml_coords(Line), Coords).
gml_shape_(element('gml:exterior',_,[Dom]), Shape) :- !,
  gml_shape_(Dom, Shape).
gml_shape_(element('gml:innerBoundaryIs',_,[Dom]), Line) :- !,
  gml_shape_(Dom, Line).
gml_shape_(element('gml:interior',_,[Dom]), Shape) :- !,
  gml_shape_(Dom, Shape).
gml_shape_(element('gml:LinearRing',_,[Dom]), 'LineString'(Line)) :- !,
  gml_shape_(Dom, Line).
gml_shape_(element('gml:MultiPolygon',_,Dom), 'MultiPolygon'(Polygons)) :- !,
  maplist(gml_shape_, Dom, Polygons).
gml_shape_(element('gml:MultiSurface',_,Dom), 'MultiSurface'(Surfaces)) :- !,
  maplist(gml_shape_, Dom, Surfaces).
gml_shape_(element('gml:outerBoundaryIs',_, [Dom]), Line) :- !,
  gml_shape_(Dom, Line).
gml_shape_(element('gml:Polygon',_,Dom), 'Polygon'(Lines)) :- !,
  % Outer boundary and -- possibly -- inner boundary.
  maplist(gml_shape_, Dom, Lines).
gml_shape_(element('gml:polygonMember',_,[Dom]), Polygon) :- !,
  gml_shape_(Dom, Polygon).
gml_shape_(element('gml:posList',_,[Coords]), Line) :- !,
  atom_phrase(gml_poslist(Line), Coords).
gml_shape_(element('gml:surfaceMember',_,[Dom]), Surface) :-
  gml_shape_(Dom, Surface).

gml_coords(['Point'([X,Y])|T]) -->
  number(X), !,
  blanks, ",", blanks,
  number(Y),
  blanks,
  gml_coords(T).
gml_coords([]) --> "".

gml_poslist(['Point'([X,Y])|T]) -->
  number(X), !,
  blank, blanks,
  number(Y),
  blanks,
  gml_poslist(T).
gml_poslist([]) --> "".
