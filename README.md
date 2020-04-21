# Geospatial support in Prolog

## Dependencies

The GEOS library must be installed system-wide:

```sh
apt install libgeos-dev     # Debian, Ubuntu
dnf install geos geos-devel # Fedora, Red Hat
```

## Installation

Run the following in [SWI-Prolog](http://www.swi-prolog.org):

```pl
pack_install(prolog_geo).
```

## Use

You can now use this library as follows:

```
?- [library(geo)].
```

And use its predicates:

```pl
?- geo_property(Property).
Property = geos_version('3.7.2-CAPI-1.11.2 b55d2125').
```
