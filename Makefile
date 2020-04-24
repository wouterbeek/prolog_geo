# -*- Makefile -*-

CXXFLAGS+=-g --std=c++17 -Wall -Wextra `geos-config --cflags`
LD=g++
LDSOFLAGS+=`geos-config --ldflags`
LIB=-lgeos_c
OBJ=$(SRC:.cpp=.o)
SOBJ=$(PACKSODIR)/geo.$(SOEXT)
SRC=$(wildcard cpp/*.cpp)

.PHONY: check clean distclean install

all: $(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $^ $(LIB) $(SWISOLIB)

cpp/%.o: cpp/%.cpp
	$(CXX) $(ARCH) $(CFLAGS) $(CXXFLAGS) -c -o $@ $<

check:
	${SWIPL} -s test/test_geo.pl -g run_tests -t halt

clean:
	$(RM) $(OBJ)

distclean:
	$(RM) $(SOBJ)

install:
