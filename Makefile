# -*- Makefile -*-

cc=g++
CFLAGS+=-std=c++17 -g -Wall -Wextra `geos-config --cflags`
ld=g++
LDSOFLAGS+=`geos-config --ldflags`
libs=-lgeos_c -lspatialindex
obj=$(src:.cpp=.o)
rm=rm -f
sobj=$(PACKSODIR)/gis.$(SOEXT)
src=$(wildcard cpp/*.cpp)

all: $(sobj)

$(sobj): $(obj)
	mkdir -p $(PACKSODIR)
	$(ld) $(ARCH) $(LDSOFLAGS) -o $@ $^ $(libs) $(SWISOLIB)

cpp/%.o: cpp/%.cpp
	$(cc) $(ARCH) $(CFLAGS) -c -o $@ $<

check:
clean:
	$(rm) $(obj)

distclean:
	$(rm) $(sobj)

install:
