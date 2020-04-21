:- module(
  geojson,
  [
    geojson_shape/2 % +Dict, -Shape
  ]
).

/** <module> GeoJSON support

*/

:- use_module(library(apply)).





%! geojson_shape(+Dict:dict, -Shape:compound) is det.

geojson_shape(Dict, shape(_Z,_LRS,_Crs,Shape)) :-
  _{coordinates: Coords, type: Type0} :< Dict,
  downcase_atom(Type0, Type),
  geojson_shape_(Type, Coords, Shape).

geojson_shape_(linestring, L, 'LineString'(Points)) :- !,
  maplist(geojson_shape_(point), L, Points).
geojson_shape_(multilinestring, L, 'MultiLineString'(Lines)) :- !,
  maplist(geojson_shape_(linestring), L, Lines).
geojson_shape_(multipoint, L, 'MultiPoint'(Points)) :- !,
  maplist(geojson_shape_(point), L, Points).
geojson_shape_(multipolygon, L, 'MultiPolygon'(Polygons)) :- !,
  maplist(geojson_shape_(polygon), L, Polygons).
geojson_shape_(point, L, 'Point'(L)) :- !.
geojson_shape_(polygon, L, 'Polygon'(Lines)) :-
  maplist(geojson_shape_(linestring), L, Lines).
