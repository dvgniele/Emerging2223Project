-module(main).
-export([init/0]).
-import(ambient, [init_world/0]).

init() -> io:fwrite("hello, world\n").