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

// gis_union_(+Wkt1:atom, +Wkt2:atom, -Wkt3:atom) is det.
PREDICATE(gis_union_, 3)
{
  size_t len1, len2;
  char *s1, *s2;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSContextHandle_t cx {GEOS_init_r()};
  GEOSWKTReader *r = GEOSWKTReader_create_r(cx);
  const GEOSGeometry *g1 = GEOSWKTReader_read_r(cx, r, s1);
  const GEOSGeometry *g2 = GEOSWKTReader_read_r(cx, r, s2);
  GEOSWKTReader_destroy_r(cx, r);
  const GEOSGeometry *g3 = GEOSUnion_r(cx, g1, g2);
  GEOSWKTWriter *w = GEOSWKTWriter_create_r(cx);
  const char *s3 {GEOSWKTWriter_write_r(cx, w, g3)};
  GEOSWKTWriter_destroy_r(cx, w);
  GEOS_finish_r(cx);
  return A3 = s3;
}

install_t install_gis()
{
}
