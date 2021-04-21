#include <cassert>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

#define PL_ARITY_AS_SIZE 1
#include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>

#define GEOS_USE_ONLY_R_API
#include <geos_c.h>
#include <proj.h>

#define CVT_TEXT (CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8)

extern "C" {
#define MKATOM(a) ATOM_ ## a = PL_new_atom(#a)

  static atom_t ATOM_geos_version;

  install_t
  install_geo(void)
  {
    MKATOM(geos_version);
  }
}

[[nodiscard]] auto atom_to_string(term_t t) -> std::string;
auto parse_geometry(const char* s) -> GEOSGeometry*;

const GEOSContextHandle_t handle{GEOS_init_r()};
GEOSWKTWriter* writer = GEOSWKTWriter_create_r(handle);

class Projection {
public:
  Projection(const std::string& fromCrs,
             const std::string& toCrs)
    : m_context{proj_context_create()}
  {
    PJ* projection = proj_create_crs_to_crs(m_context,
                                            fromCrs.c_str(),
                                            toCrs.c_str(),
                                            nullptr);
    if (projection == 0) {
      //<< proj_context_errno_string(m_context, proj_errno(projection))
      std::cerr << "Unable to project: " << proj_errno(projection) << '\n';
    }
    m_projection = proj_normalize_for_visualization(m_context, projection);
    if (m_projection == 0)  {
      //<< proj_context_errno_string(m_context, proj_errno(m_projection))
      std::cerr << "Unable to project: " << proj_errno(m_projection) << '\n';
    }
    proj_destroy(projection);
    //proj_context_errno_string(m_context, proj_context_errno(m_context))
  }

  ~Projection()
  {
    proj_destroy(m_projection);
    proj_context_destroy(m_context);
  }

  [[nodiscard]] inline auto transform(const PlTerm& term) const -> PlCompound
  {
    const PJ_COORD from{proj_coord(term[1],
                                   term[2],
                                   term.arity() >= 3 ? static_cast<double>(term[3]) : 0.0,
                                   term.arity() == 4 ? static_cast<double>(term[3]) : 0.0)};
    const PJ_COORD to{proj_trans(m_projection, PJ_FWD, from)};
    switch (term.arity()) {
    case 2:
      return{PlCompound("coord", PlTermv(to.xyz.x, to.xyz.y))};
    case 3:
      return{PlCompound("coord", PlTermv(to.xyz.x, to.xyz.y, to.xyz.z))};
    default:
      std::cerr << "No support for this coord arity.\n";
      std::abort();
    }
  }

private:
  PJ_CONTEXT* m_context = nullptr;
  PJ* m_projection = nullptr;
};

PREDICATE(geo_transform_coords_, 4) {
  static Projection projection{atom_to_string(A1), atom_to_string(A3)};
  PlTail froms(A2);
  PlTerm from;
	PlTermv av(1);
	PlTail tos(av[0]);
  while (froms.next(from)) {
    tos.append(projection.transform(from));
	}
	tos.close();
	return{static_cast<foreign_t>(A4 = av[0])};
}

// geo_halt_ is det.
PREDICATE(geo_halt_, 0) {
  GEOS_finish_r(handle);
  PL_succeed;
}

// geo_property_(?Property:compound) is det.
PREDICATE(geo_property_, 1) {
  atom_t optionName;
  std::size_t optionArity{0};
  if (PL_get_name_arity(A1, &optionName, &optionArity) && optionArity == 1) {
    PlTerm optionValue{A1[1]};
    if (optionName == ATOM_geos_version) {
      return{static_cast<foreign_t>(optionValue = GEOSversion())};
    } else {
      throw PlDomainError("geo_property", A1);
    }
  }
  return{static_cast<foreign_t>(PL_type_error("compound", A1))};
}

// wkt_boundary_(+Wkt:atom, -Boundary:atom) is det.
PREDICATE(wkt_boundary_, 2) {
  std::size_t len{0};
  char* s1 = nullptr;
  if (!PL_get_nchars(A1, &len, &s1, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = GEOSBoundary_r(handle, g1);
  const char* s2 = GEOSWKTWriter_write_r(handle, writer, g2);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A2 = s2)};
}

// wkt_contains_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_contains_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSContains_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_convex_hull_(+Wkt:atom, -ConvexHull:atom) is det.
PREDICATE(wkt_convex_hull_, 2) {
  std::size_t len{0};
  char* s1 = nullptr;
  if (!PL_get_nchars(A1, &len, &s1, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = GEOSConvexHull_r(handle, g1);
  const char* s2 = GEOSWKTWriter_write_r(handle, writer, g2);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A3 = s2)};
}

// wkt_crosses_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_crosses_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSCrosses_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_difference_(+Wkt1:atom, +Wkt2:atom, -Wkt3:atom) is det.
PREDICATE(wkt_difference_, 3) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const GEOSGeometry* g3 = GEOSDifference_r(handle, g1, g2);
  const char* s3 = GEOSWKTWriter_write_r(handle, writer, g3);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A3 = s3)};
}

// wkt_disjoint_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_disjoint_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSDisjoint_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_distance_(+Wkt1:atom, +Wkt2:atom, -Distance:float) is semidet.
PREDICATE(wkt_distance_, 3) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  double dist{0.0};
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const int rc{GEOSDistance_r(handle, g1, g2, &dist)};
  if (rc == 1) {
    return{static_cast<foreign_t>(A3 = dist)};
  } else {
    PL_fail;
  }
}

// wkt_envelope_(+Wkt1:atom, -Envelope:atom) is det.
PREDICATE(wkt_envelope_, 2) {
  std::size_t len{0};
  char* s1 = nullptr;
  if (!PL_get_nchars(A1, &len, &s1, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = GEOSEnvelope_r(handle, g1);
  const char* s2 = GEOSWKTWriter_write_r(handle, writer, g2);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A2 = s2)};
}

// wkt_equals_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_equals_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSEquals_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_intersection_(+Wkt1:atom, +Wkt2:atom, -Wkt3:atom) is det.
PREDICATE(wkt_intersection_, 3) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const GEOSGeometry* g3 = GEOSIntersection_r(handle, g1, g2);
  const char* s3 = GEOSWKTWriter_write_r(handle, writer, g3);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A3 = s3)};
}

// wkt_intersects_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_intersects_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSIntersects_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_overlaps_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_overlaps_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSOverlaps_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_symmetric_difference_(+Wkt1:atom, +Wkt2:atom, -Wkt3:atom) is det.
PREDICATE(wkt_symmetric_difference_, 3) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const GEOSGeometry* g3 = GEOSSymDifference_r(handle, g1, g2);
  const char* s3 = GEOSWKTWriter_write_r(handle, writer, g3);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A3 = s3)};
}

// wkt_touches_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_touches_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSTouches_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// wkt_union_(+Wkt1:atom, +Wkt2:atom, -Wkt3:atom) is det.
PREDICATE(wkt_union_, 3) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const GEOSGeometry* g3 = GEOSUnion_r(handle, g1, g2);
  const char* s3 = GEOSWKTWriter_write_r(handle, writer, g3);
  GEOSWKTWriter_destroy_r(handle, writer);
  return{static_cast<foreign_t>(A3 = s3)};
}

// wkt_within_(+Wkt1:atom, +Wkt2:atom) is semidet.
PREDICATE(wkt_within_, 2) {
  std::size_t len1{0}, len2{0};
  char* s1 = nullptr;
  char* s2 = nullptr;
  if (!PL_get_nchars(A1, &len1, &s1, CVT_ATOM) ||
      !PL_get_nchars(A2, &len2, &s2, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* g1 = parse_geometry(s1);
  const GEOSGeometry* g2 = parse_geometry(s2);
  const char status{GEOSWithin_r(handle, g1, g2)};
  switch (status) {
  case 0:
    PL_fail;
  case 1:
    PL_succeed;
  default:
    return{static_cast<foreign_t>(PL_existence_error("geos_status",
                                                     static_cast<term_t>(status)))};
  }
}

// shape_type_(+Wkt:atom, -Type:atom) is det.
PREDICATE(shape_type_, 2) {
  std::size_t len{0};
  char* buffer = nullptr;
  if (!PL_get_nchars(A1, &len, &buffer, CVT_ATOM)) {
    PL_fail;
  }
  const GEOSGeometry* geometry = parse_geometry(buffer);
  return{static_cast<foreign_t>(A2 = GEOSGeomType_r(handle, geometry))};
}

auto atom_to_string(term_t t) -> std::string
{
  std::size_t len{0};
  char* buffer = nullptr;
  assert(PL_get_nchars(t, &len, &buffer, CVT_TEXT));
  return{buffer};
}

auto parse_geometry(const char* const buffer) -> GEOSGeometry*
{
  GEOSWKTReader* reader = GEOSWKTReader_create_r(handle);
  GEOSGeometry* geometry = GEOSWKTReader_read_r(handle, reader, buffer);
  if (!geometry) {
    throw PlTypeError("geometry", buffer);
  }
  GEOSWKTReader_destroy_r(handle, reader);
  return geometry;
}
