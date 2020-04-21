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
