# -*- Makefile -*-
cc=g++
CFLAGS+=-std=c++17 -g -Wall -Wextra $(geos_cflags)
geos_cflags=`geos-config --cflags`
geos_ldflags=`geos-config --ldflags`
geos_lib=-lgeos_c
ld=g++
libs=$(geos_lib)
obj=$(src:.cpp=.o)
rm=rm -f
sobj=$(PACKSODIR)/gis.$(SOEXT)
src=$(wildcard cpp/*.cpp)

all: $(sobj)

$(sobj): $(obj)
	mkdir -p $(PACKSODIR)
	$(ld) $(ARCH) $(LDSOFLAGS) $(geos_ldflags) -o $@ $^ $(libs) $(SWISOLIB)

cpp/%.o: cpp/%.cpp
	$(cc) $(ARCH) $(CFLAGS) -c -o $@ $<

check:

clean:
	$(rm) $(obj)

distclean:
	$(rm) $(sobj)

install:
