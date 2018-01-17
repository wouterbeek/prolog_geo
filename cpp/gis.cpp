#define PL_ARITY_AS_SIZE 1
#include <cstdio>
#include <cstdlib>
#include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>

//DEB
#include <iostream>

// GEOS C API
#define GEOS_USE_ONLY_R_API
#include <geos_c.h>

static const PlAtom ATOM_geos_version {"geos_version"};

GEOSGeometry *parse_geometry(const GEOSContextHandle_t handle, const char *s);

// gis_property_(?Property:compound) is det.
PREDICATE(gis_property_, 1)
{
  PlTerm value {A1[1]};
  if (A1.name() == ATOM_geos_version) {
    return (value = GEOSversion());
  } else {
    throw PlDomainError("gis_property", A1);
  }
}

// gis_contains_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(gis_contains_, 2)
{
  size_t len1, len2;
  char *s1, *s2;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g1 = parse_geometry(handle, s1);
  const GEOSGeometry *g2 = parse_geometry(handle, s2);
  const char status {GEOSContains_r(handle, g1, g2)};
  GEOS_finish_r(handle);
  switch(status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    std::cerr << "Cannot determine whether or not two shapes touch.\n";
    PL_fail;
  }
}

// gis_distance_(+Wkt1:atom, +Wkt2:atom, -Distance:float) is semidet.
PREDICATE(gis_distance_, 3)
{
  size_t len1, len2;
  char *s1, *s2;
  double dist {0.0};
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g1 = parse_geometry(handle, s1);
  const GEOSGeometry *g2 = parse_geometry(handle, s2);
  int rc = GEOSDistance_r(handle, g1, g2, &dist);
  GEOS_finish_r(handle);
  if (rc == 1) {
    return (A3 = dist);
  } else {
    PL_fail;
  }
}

// gis_intersects_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(gis_intersects_, 2)
{
  size_t len1, len2;
  char *s1, *s2;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g1 = parse_geometry(handle, s1);
  const GEOSGeometry *g2 = parse_geometry(handle, s2);
  const char status {GEOSIntersects_r(handle, g1, g2)};
  GEOS_finish_r(handle);
  switch(status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    std::cerr << "Cannot determine whether or not two shapes intersect.\n";
    PL_fail;
  }
}

// gis_touches_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(gis_touches_, 2)
{
  size_t len1, len2;
  char *s1, *s2;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g1 = parse_geometry(handle, s1);
  const GEOSGeometry *g2 = parse_geometry(handle, s2);
  const char status {GEOSTouches_r(handle, g1, g2)};
  GEOS_finish_r(handle);
  switch(status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    std::cerr << "Cannot determine whether or not two shapes touch.\n";
    PL_fail;
  }
}

// gis_union_(+Wkt1:atom, +Wkt2:atom, -Wkt3:atom) is det.
PREDICATE(gis_union_, 3)
{
  size_t len1, len2;
  char *s1, *s2;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g1 = parse_geometry(handle, s1);
  const GEOSGeometry *g2 = parse_geometry(handle, s2);
  const GEOSGeometry *g3 = GEOSUnion_r(handle, g1, g2);
  GEOSWKTWriter *w = GEOSWKTWriter_create_r(handle);
  const char *s3 {GEOSWKTWriter_write_r(handle, w, g3)};
  GEOSWKTWriter_destroy_r(handle, w);
  return A3 = s3;
  GEOS_finish_r(handle);
}

// gis_within_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(gis_within_, 2)
{
  size_t len1, len2;
  char *s1, *s2;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g1 = parse_geometry(handle, s1);
  const GEOSGeometry *g2 = parse_geometry(handle, s2);
  const char status {GEOSWithin_r(handle, g1, g2)};
  GEOS_finish_r(handle);
  switch(status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    std::cerr << "Cannot determine whether or not two shapes touch.\n";
    PL_fail;
  }
}

// shape_type_(+Wkt:atom, -Type:atom) is det.
PREDICATE(shape_type_, 2)
{
  size_t len;
  char *s;
  if (!PL_get_nchars(A1, &len, &s, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t handle {GEOS_init_r()};
  const GEOSGeometry *g = parse_geometry(handle, s);
  int rc = (A2 = GEOSGeomType_r(handle, g));
  GEOS_finish_r(handle);
  return rc;
}

GEOSGeometry *parse_geometry(const GEOSContextHandle_t handle, const char *s)
{
  GEOSWKTReader *r = GEOSWKTReader_create_r(handle);
  GEOSGeometry *g = GEOSWKTReader_read_r(handle, r, s);
  if (g == NULL) {
    throw PlTypeError("geometry", s);
  }
  GEOSWKTReader_destroy_r(handle, r);
  return g;
}

extern "C" {
  install_t install_gis()
  {
  }
}
