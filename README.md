# Geospatial support in Prolog

## Dependencies

  1. Install [SWI-Prolog](https://www.swi-prolog.org).

  2. Install the [GEOS](https://trac.osgeo.org/geos) library.  The following works on Ubuntu:

```sh
apt install libgeos-dev
```

  3. Install the [Proj](https://proj.org) library.  The following works on Ubuntu:

```sh
apt install libproj-dev
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
Property = geos_version('3.8.1-CAPI-1.13.3').
```
