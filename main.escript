#!/usr/bin/env escript
-module(main).

-export([main/1]).

main(Args) ->
  case Args of
    [] ->
      io:put_chars(standard_error, "Error: missing argument of filename.\n"),
      erlang:halt(1);
    [Filename] ->
      {ok, Bin} = file:read_file(Filename),
      Res = randomart:digest(Bin),
      randomart:print_board(Res)
  end.

