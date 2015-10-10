%% -*- coding: utf-8 -*-
%% module lens implements functional lenses for erlang
-module(lens).

-include("lens.hrl").

%% The "invalid" state
-export([omega/0]).

%% Lens constructors
-export([h_tuple/1, h_plist/1, h_json_jsx/1, h_json_jiffy/1, h_map/1]).
-export([id/0, const/2]).

%% Lens algebra
-export([c/1, c/2]).

%% Tooling functions
-export([v/1, s/1, o/2, o/3]).

omega() -> ?OMEGA.

%% The lens module implements a concept called "Functional Lenses" for Erlang. The crucial idea
%% of these is to implement a way to abstract over data, given a functional accessor/mutator
%% construction. That is:
%% 
%% * Like an accessor/mutator pair in imperative OO languages, but
%% * Is functional and algebraically composable
%% 
%% This module implements such lenses in the setting of the Erlang programming language

%% Rule:
%%   There is one rule for the use of this module. The notion 'omega' (Omega) is treated
%%   specially as the "undefined" value which can be used as a base constructor.

%% LENSES
%% ------------------------------------------------

%% h_map/1 provides a lens accessor for a map.
% h_map(K) ->
%     #lens {
% 	g = fun(M) -> maps:get(K, M) end,
% 	s = fun(X, M) -> maps:put(K, X, M) end
%     }.
% 
% plunge(N) ->
%     #lens {
% 	g = fun(C) -> maps:put(N, C, #{}) end,
% 	s = fun(X, _M) -> maps:get(N, X) end
%     }.


%% tuple/1 provides a lens accessor for a tuple. The parameter given is the element to access.
h_tuple(N) when is_integer(N) ->
    #lens {
      g = fun(R) -> element(N, R) end,
      s = fun(X, R) -> setelement(N, R, X) end
    }.

%% plist/1 provides a lens for proplists
h_plist(Key) ->
    #lens {
      g = fun(R) ->
      		case lists:keyfind(Key, 1, R) of
      		    false -> undefined;
      		    {_, V} -> V
      		end
      	  end,
      s = fun(X, R) -> lists:keystore(Key, 1, R, {Key, X}) end
    }.
    
h_map(K) ->
    #lens {
        g = fun(M) -> maps:get(K, M, undefined) end,
        s = fun(X, M) -> maps:put(K, X, M) end
    }.

%% json_jsx/1 provides a lens for JSX json-like structures
h_json_jsx(K) ->
    G = fun
          ([{}]) -> error(badarg);
          ([{A, _} | _] = R) when is_list(R), is_atom(A) orelse is_binary(A) -> element(2, lists:keyfind(K, 1, R))
        end,
    S = fun
          (X, 'omega') -> [{K, X}];
          (X, [{}]) -> [{K, X}];
          (X, R) when is_list(R) -> lists:keystore(K, 1, R, {K, X})
        end,
    #lens { g = G, s = S }.

%% json_jiffy/1 provides a lens for Jiffy/msgpack based json-like structures
h_json_jiffy(K) ->
    G = fun
          ({[]}) -> error(badarg);
          ({[{A, _} | _] = R}) when is_list(R), is_atom(A) orelse is_binary(A) -> element(2, lists:keyfind(K, 1, R))
        end,
    S = fun
          (X, 'omega') -> {[{K, X}]};
          (X, {[]}) -> {[{K, X}]};
          (X, {R}) when is_list(R) -> {lists:keystore(K, 1, R, {K, X})}
        end,
    #lens { g = G, s = S }.

id() ->
    #lens { g = fun(X) -> X end, s = fun(_X, R) -> R end }.

const(V, D) ->
    #lens { g = fun(_) -> V end,
            s = fun ('omega', _R) -> D;
                    (_X, R) -> R
                end }.

%% ALGEBRAIC FUNCTIONS
%% -----------------------------------------------

c(#lens { g = LG, s = LP }, #lens { g = KG, s = KP }) ->
    #lens { g = fun(R) -> KG(LG(R)) end, s = fun(A, R) -> LP(KP(A, LG(R)), R) end}.
   
c([Lens]) -> Lens;
c([X, Y | Rest]) -> c([c(X, Y) | Rest]).

%% TOOLING FUNCTIONS
%% -----------------------------------------------

%% v/1 views data through a lens
v(#lens { g = G }) -> G.

%% s/1 provides the setter of the lens
s(#lens { s = S}) -> S.

%% o/2,3 provides the `over' function for lenses
o(#lens { g = G, s = S }, F) -> fun(X) -> S(F(G(X)), X) end.
o(#lens { g = G, s = S }, F, X) -> S(F(G(X)), X).

-ifdef(TEST).

%% Omega rules for lenses which has 'omega'-like-behaviour:
% lens_omega_1(#lens { g = Get }) ->
%     'omega' == Get('omega').
%     
% lens_omega_2(Val, #lens { s = Put }) ->
%     ?FORALL(V, 'omega'(Val),
%         'omega' == Put('omega', V)).
%         
% lens_omega_3(Val, #lens { g = Get }) ->
%     ?FORALL(V, Val,
%         'omega' /= Get(V)).
%         
% lens_omega_4(Gen, Val, #lens { s = Put}) ->
%     ?FORALL({X, R}, {'omega'(Val), Gen},
%       Put(X, R) /= 'omega').

prop_proplist_1() ->
	?FORALL(Keyword, g_keyword(),
	  begin
	      lens_prop_getput(g_proplist(), h_plist(Keyword))
	  end).

prop_proplist_2() ->
	?FORALL(PL, g_proplist(),
	  begin
	  	case [KW || {KW, _} <- PL] of
	  	  [] -> true;
	  	  Keywords ->
	  	    ?LET(KW, oneof(Keywords),
	  	         lens_prop_putget_omega(PL, int(), h_plist(KW)))
	  	end
	  end).

prop_proplist_3() ->
	?FORALL(Keyword, g_keyword(),
	  begin
	      lens_prop_putput(g_proplist(), int(), h_plist(Keyword))
	  end).

properties_test() ->
	[] = eqc:module(?MODULE),
	ok.

-endif.
