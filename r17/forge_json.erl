-module(forge_json).

-export([from_jsx/1, to_jsx/1]).

%% from_jsx/1 converts a structure in JSX form into an Erlang map. Assumes the format is correct
from_jsx(I) when is_integer(I) -> I;
from_jsx(N) when is_number(N) -> N;
from_jsx(B) when is_binary(B) -> B;
from_jsx([{_, _} | _] = Object) ->
	maps:from_list(Object);
from_jsx(L) when is_list(L) ->
	[from_jsx(Item) || Item <- L].

to_jsx(N) when is_number(N) -> N;
to_jsx(B) when is_binary(B) -> B;
to_jsx(M) when is_map(M) -> maps:to_list(M);
to_jsx(L) when is_list(L) ->
	[to_jsx(Item) || Item <- L].

