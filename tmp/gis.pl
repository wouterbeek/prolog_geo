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
  ]
).

/** <module> GIS

@author Wouter Beek
@version 2018/01
*/





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



