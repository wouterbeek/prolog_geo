# -*- Makefile -*-

CXXFLAGS+=-I/usr/local/include -Wall -Wcast-align -Wconversion -Wdouble-promotion -Wduplicated-branches -Wduplicated-cond -Wextra -Wformat=2 -Wlogical-op -Wmisleading-indentation -Wnon-virtual-dtor -Wnull-dereference -Wold-style-cast -Woverloaded-virtual -Wpedantic -Wshadow -Wsign-conversion -Wunused -Wuseless-cast -fPIC -g -march=native -shared --std=c++20 `geos-config --cflags`
LD=g++
LDSOFLAGS+=`geos-config --ldflags`
LDSOFLAGS+=-L/usr/local/lib
LIB=-lgeos_c -lproj
OBJ=$(SRC:.cpp=.o)
SOBJ=$(PACKSODIR)/geo.$(SOEXT)
SRC=cpp/geo.cpp

.PHONY: all check clean distclean install

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $^ $(LIB) $(SWISOLIB)

cpp/%.o: cpp/%.cpp
	$(CXX) $(ARCH) $(CFLAGS) $(CXXFLAGS) -c -o $@ $<

all: $(SOBJ)

check:
	LANG=C.UTF-8 $(SWIPL) -s test/test_geo.pl -g run_tests -t halt

clean:
	$(RM) $(OBJ)

distclean:
	$(RM) $(OBJ) $(SOBJ)

install:
