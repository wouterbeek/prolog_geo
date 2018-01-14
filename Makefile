# -*- Makefile -*-
CC=gcc
CFLAGS+=-std=c++17 -g -Wall $(GEOS_CFLAGS)
GEOS_CFLAGS=`geos-config --cflags`
GEOS_LDFLAGS=`geos-config --ldflags`
GEOS_LIB=-lgeos_c
LD=gcc
LIBS=$(GEOS_LIB)
OBJ=$(SRC:.cpp=.o)
RM=rm -f
SOBJ=$(PACKSODIR)/gis.$(SOEXT)
SRC=$(wildcard cpp/*.cpp)

all: $(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) $(GEOS_LDFLAGS) -o $@ $^ $(LIBS) $(SWISOLIB)

cpp/%.o: cpp/%.cpp
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ $<

check:

clean:
	$(RM) $(OBJ)

distclean:
	$(RM) $(SOBJ)

install:
