.PHONY: clean distclean default

CXX=c++
CXXFLAGS= -std=c++11

default: tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer: lexer.cpp parser.hpp
	$(CXX) $(CXXFLAGS) -o lexer lexer.cpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

symbol.o: symbol.cpp
	$(CXX) $(CXXFLAGS) -c symbol.cpp

error.o: error.cpp
	$(CXX) $(CXXFLAGS) -c error.cpp

general.o: general.cpp
	$(CXX) $(CXXFLAGS) -c general.cpp

error.o    : error.cpp error.h general.h
symbol.o   : symbol.cpp symbol.h error.h general.h
general.o	 : general.cpp general.h error.h

parser.o: parser.cpp lexer.hpp parser.hpp symbol.h

lexer.o: lexer.cpp lexer.hpp parser.hpp symbol.h ast.hpp

tony: lexer.o parser.o symbol.o error.o general.o
	$(CXX) $(CXXFLAGS) -o tony lexer.o parser.o symbol.o error.o general.o

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o tony
