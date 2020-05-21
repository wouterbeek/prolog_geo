# Geospatial support in Prolog

## Dependencies

  1. Install [SWI-Prolog](https://www.swi-prolog.org).

  2. Install the GEOS library:

```sh
apt install libgeos-dev     # Debian, Ubuntu
dnf install geos geos-devel # Fedora, Red Hat
```

## Installation

Install this library:

```sh
swipl -g 'pack_install(prolog_geo)' -t halt
```

## Use

Once installed, modules from this library are loaded as follows:

```
?- [library(geo)].
```

Predicates defined in the loaded modules can now be used:

```pl
?- geo_property(Property).
Property = geos_version('3.7.2-CAPI-1.11.2 b55d2125').
```
