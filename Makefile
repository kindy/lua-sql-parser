include make.config

.PHONY=test

all: sql_parser

sql_parser: src/sql_parser.so

#src/sql_parser.so: src/sql_parser.o src/xsql.o src/xsql.tab.o
#	$(CC) $(CFLAGS) $(LIB_OPTION) -o src/sql_parser.so src/sql_parser.o src/xsql.o src/xsql.tab.o

src/xsql.o: src/xsql.c src/xsql.tab.h

src/xsql.tab.o: src/xsql.tab.c src/xsql.tab.h

xsql: src/xsql.tab.c src/xsql.c

src/xsql.tab.c src/xsql.tab.h:	src/xsql.y
	cd src && ${YACC} -v xsql.y

src/xsql.c:	src/xsql.l
	${LEX} -o $@ $<


t_parser: src/sql_parser.so
	if [ ! -f sql_parser.so ]; then ln -sf src/sql_parser.so ./; fi

src/sql_parser.so: src/t_parser.o src/xt.o src/xt.tab.o src/xt.fn.o
	$(CC) $(CFLAGS) $(LIB_OPTION) -o src/sql_parser.so src/t_parser.o src/xt.o src/xt.tab.o src/xt.fn.o

src/t_parser.o: src/sql_parser.c
	$(CC) $(CFLAGS) -c -o src/t_parser.o src/sql_parser.c

src/xt.o: src/xt.c src/xt.tab.h

src/xt.tab.o: src/xt.tab.c src/xt.tab.h

xt: src/xt.tab.c src/xt.c

src/xt.tab.c src/xt.tab.h:	src/xt.y
	cd src && ${YACC} -v xt.y

src/xt.c:	src/xt.l
	${LEX} -o $@ $<

t: t_parser
	LUA_CPATH="/opt/openresty/lualib/?.so;;" lua t/t_parser.lua
