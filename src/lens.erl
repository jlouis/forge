%% module lens implements functional lenses for erlang
-module(lens).

-include("lens.hrl").

%% Lens constructors
-export([tuple/1, plist/1, json_jsx/1, json_jiffy/1]).

%% Lens algebra
-export([c/1, c/2]).

%% Tooling functions
-export([v/1]).

% The lens module implements a concept called "Functional Lenses" for Erlang. The crucial idea of these is
% to implement a way to abstract over data, given a functional accessor/mutator construction. That is:
% 
% * Like an accessor/mutator pair in imperative OO languages, but⋯
% * Is functional and algebraically composable
% 
% This module implements such lenses in the setting of the Erlang programming language

%% LENSES
%% ------------------------------------------------

%% tuple/1 provides a lens accessor for a tuple. The parameter given is the element to access.
tuple(N) when is_integer(N) ->
    #lens {
      g = fun(R) -> element(N, R) end,
      s = fun(X, R) -> setelement(N, R, X) end
    }.
    
%% plist/1 provides a lens for proplists
plist(Key) ->
    #lens {
      g = fun(R) -> element(2, lists:keyfind(Key, 1, R)) end,
      s = fun(X, R) -> lists:keystore(Key, 1, R, {Key, X}) end
    }.
    
%% json_jsx/1 provides a lens for JSX json-like structures
json_jsx(K) ->
    G = fun
          ([{}]) -> error(badarg);
          ([{A, _} | _] = R) when is_list(R), is_atom(A) orelse is_binary(A) -> element(2, lists:keyfind(K, 1, R))
        end,
    S = fun
          (X, [{}]) -> [{K, X}];
          (X, R) when is_list(R) -> lists:keystore(K, 1, R, {K, X})
        end,
    #lens { g = G, s = S }.

%% json_jiffy/1 provides a lens for Jiffy/msgpack based json-like structures
json_jiffy(K) ->
    G = fun
          ([{}]) -> error(badarg);
          ({[{A, _} | _] = R}) when is_list(R), is_atom(A) orelse is_binary(A) -> element(2, lists:keyfind(K, 1, R))
        end,
    S = fun
          (X, [{}]) -> [{K, X}];
          (X, {R}) when is_list(R) -> {lists:keystore(K, 1, R, {K, X})}
        end,
    #lens { g = G, s = S }.

%% Algebraic functions
c(#lens { g = LG, s = LP }, #lens { g = KG, s = KP }) ->
    #lens { g = fun(R) -> KG(LG(R)) end, s = fun(A, R) -> LP(KP(A, LG(R)), R) end}.
   
c([Lens]) -> Lens;
c([X, Y | Rest]) -> c([c(X, Y) | Rest]).

%% TOOLING FUNCTIONS
%% -----------------------------------------------

%% v/1 views data through a lens
v(#lens { g = G }) -> fun(R) -> G(R) end.
