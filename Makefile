.PHONY: clean distclean default

CXX=c++
CXXFLAGS= -std=c++11

default: compiler

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer: lexer.cpp parser.hpp
	$(CXX) $(CXXFLAGS) -o lexer lexer.cpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

compiler: parser.cpp

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean:
	$(RM) lexer
