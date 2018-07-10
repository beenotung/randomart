-module(randomart).

-include_lib("eunit/include/eunit.hrl").

-export([
  main/1
  , digest/1
  , print_board/1
]).

%%%%%%%
% API %
%%%%%%%

main(Args) ->
  case Args of
    [] ->
      io:put_chars(standard_error, "Error: missing argument of filename.\n"),
      erlang:halt(1);
    [Filename] ->
      {ok, Bin} = file:read_file(Filename),
      Res = digest(Bin),
      print_board(Res)
  end.

digest(Bin) when is_binary(Bin) ->
  Board = init(),
  Hash = crypto:hash(sha256, Bin),
  loop(Board, Hash).

%%%%%%%%
% test %
%%%%%%%%

digest_test() ->
  Hex = "18ff18d7f4a6d8ceddd4070ee2c5f845",
  Bin = hexstr_to_bin(Hex),
  io:format("~p~n", [Bin]),
  Board = digest(Bin),
  print_board(Board).

hexstr_to_bin(S) ->
  list_to_binary(hexstr_to_list(S)).
hexstr_to_list([X, Y | T]) ->
  [int(X) * 16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
  [].
int(C) when $0 =< C, C =< $9 ->
  C - $0;
int(C) when $A =< C, C =< $F ->
  C - $A + 10;
int(C) when $a =< C, C =< $f ->
  C - $a + 10.

%%%%%%%%%%%%%%%%%%%%%%
% internal functions %
%%%%%%%%%%%%%%%%%%%%%%

-define(N_ROW, 9).
-define(N_COL, 17).
-record(board, {
  cells
  , start_point
  , cur_point
}).
-define(NEW_LINE, 10).

from_rc(Row, Col) ->
  erlang:round(Row * ?N_COL + Col).

to_rc(Point) ->
  Row = Point div ?N_COL,
  Col = Point rem ?N_COL,
  [Row, Col].

rc_test() ->
  Point = 34,
  [Row, Col] = to_rc(Point),
  Point = from_rc(Row, Col).

init() ->
  Center = from_rc((?N_ROW - 1) / 2, (?N_COL - 1) / 2),
  Cells = array:new([
    {fixed, true}
    , {default, 0}
    , {size, ?N_ROW * ?N_COL}
  ]),
  #board{
    cells = Cells
    , start_point = Center
    , cur_point = Center
  }.

loop(Board, <<>>) ->
  Board;
loop(Board, <<Code:2, Tail/bitstring>>) ->
  [Row, Col] = to_rc(Board#board.cur_point),
  [Row1, Col1] =
    case Code of
      0 -> [Row - 1, Col - 1];
      1 -> [Row - 1, Col + 1];
      2 -> [Row + 1, Col - 1];
      3 -> [Row + 1, Col + 1]
    end,
  New_Row =
    case Row1 of
      -1 -> 0;
      ?N_ROW -> ?N_ROW - 1;
      _ -> Row1
    end,
  New_Col =
    case Col1 of
      -1 -> 0;
      ?N_COL -> ?N_COL - 1;
      _ -> Col1
    end,
  New_Cur_Point = from_rc(New_Row, New_Col),
  Old_Cells = Board#board.cells,
  Count = array:get(New_Cur_Point, Old_Cells) + 1,
  New_Cells = array:set(New_Cur_Point, Count, Old_Cells),
  New_Board = Board#board{
    cur_point = New_Cur_Point
    , cells = New_Cells
  },
  loop(New_Board, Tail).

print_board(Board) ->
  io:format("+-----------------+~n"),
  Cells_List = array:to_list(Board#board.cells),
  Cells_S = format_cells(Cells_List, Board#board.start_point, Board#board.cur_point),
  io:format("~s", [Cells_S]),
  io:format("+----[SHA256]-----+~n").

format_cells(Cell_List, Start, End) ->
  S = format_cells(Cell_List, Start, End, [], 0),
  lists:reverse(S).
format_cells([], _Start, _End, Acc, _Idx) ->
  Acc;
format_cells([Freq | T], Start, End, Acc, Idx) ->
  C =
    case Idx of
      Start -> $S;
      End -> $E;
      _ -> freq_to_char(Freq)
    end,
  New_Acc =
    case Idx rem ?N_COL of
      0 -> [C, $| | Acc];
      ?N_COL - 1 -> [?NEW_LINE, $|, C | Acc];
      _ -> [C | Acc]
    end,
  format_cells(T, Start, End, New_Acc, Idx + 1).

freq_to_char(Freq) when Freq >= 0 ->
  case Freq of
    0 -> $ ;
    1 -> $.;
    2 -> $o;
    3 -> $+;
    4 -> $=;
    5 -> $*;
    6 -> $B;
    7 -> $O;
    8 -> $X;
    9 -> $@;
    10 -> $%;
    11 -> $&;
    12 -> $#;
    13 -> $/;
    _  -> $^
  end.
