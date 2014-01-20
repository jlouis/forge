%% -*- coding: utf-8 -*-
%% module lens implements functional lenses for erlang
-module(lens).

-include("lens.hrl").

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-export([prop_true/0, prop_tuple_1/0, prop_tuple_2/0, prop_tuple_3/0]).
-export([prop_proplist_1/0, prop_proplist_2/0, prop_proplist_3/0 ]).
-endif.

%% Lens constructors
-export([h_tuple/1, h_plist/1, h_json_jsx/1, h_json_jiffy/1]).
-export([id/0, const/2]).

%% Lens algebra
-export([c/1, c/2]).

%% Tooling functions
-export([v/1, s/1, o/2, o/3]).

%% The lens module implements a concept called "Functional Lenses" for Erlang. The crucial idea
%% of these is to implement a way to abstract over data, given a functional accessor/mutator
%% construction. That is:
%% 
%% * Like an accessor/mutator pair in imperative OO languages, but⋯
%% * Is functional and algebraically composable
%% 
%% This module implements such lenses in the setting of the Erlang programming language

%% Rule:
%%   There is one rule for the use of this module. The notion 'omega' (Omega) is treated
%%   specially as the "undefined" value which can be used as a base constructor.
%%   Yes, it is the unicode GREEK CAPITL LETTER OMEGA U+03A9, and Erlang understands
%%   such unicode entries.

%% LENSES
%% ------------------------------------------------

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
      s = fun
            (X, 'omega') -> [{Key, X}];
            (X, R) -> lists:keyreplace(Key, 1, R, {Key, X})
          end
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

%% Lens generators

%% A list of atom keywords
g_keyword() ->
	oneof([
		'baby_Ñandú',
		black_mamba,
		blob_fish,
		brilleabe,
		cheetah,
		condor,
		crocodile,
		drilleabe,
		elephant,
		flamingo,
		giraffe,
		greater_rhea,
		hammerhead_shark,
		hippo,
		kiwi,
		'kaffebøffel',
		komodo_dragon,
		leopard,
		lion,
		parrot,
		polar_bear,
		pufferfish,
		rhino,
		rhinoceros_hornbill,
		scorpion,
		sea_turtle,
		sloth,
		tarantula,
		tucan,
		unicorn,
		werewolf,
		zebra
		]).

%% A list of binary keywords
g_star_sign() ->
	oneof(
		[atom_to_binary(A, utf8) ||
		  A <- [rat, ox, tiger, rabbit, dragon, snake, horse, sheep, monkey, rooster, dog, pig]]).

g_proplist() -> list({oneof([g_keyword(), g_star_sign()]), int()}).
		
%% g_json_jsx() -> g_proplist().

%% Tuples of size K
g_tuple(K) ->
	list_to_tuple(vector(K, int())).

'omega'(Gen) ->
    frequency([{100, Gen}, {10, return('omega')}]).

%% The three properties of lenses:

%% Lenses have three properties. If we get out a value and then
%% subsequently put it, nothing should change.
lens_prop_getput(Gen, #lens { g = Get, s = Put}) ->
    ?FORALL(X, Gen,
            X == Put(Get(X), X)).


%% And if we put in a value and then get the value out, we should get
%% the value we just put.
lens_prop_putget(Gen, Val, #lens { g = Get, s = Put}) ->
    ?FORALL({X, A}, {Gen, Val},
            A == Get(Put(A, X))).

lens_prop_putget_omega(Gen, Val, #lens { g = Get, s = Put}) ->
    ?FORALL({X, A}, {'omega'(Gen), Val},
            A == Get(Put(A, X))).
%% The point is that we would like to *PROVE* the above properties for
%% the lenses we derive. But proofs are large, boring and hard. Hence,
%% we simply provide a tool which can test our lens access via
%% QuickCheck. I use EQC here, but PropEr would also do. The deeper
%% idea is that we will probalistically make our lens correct by
%% probing it with random code.

%% The above two definitions makes for a well-behaved lens. A very
%% well behaved lens also satisfies this last rule: Last put wins and
%% behaves as if it was the only thing done to the lens.
lens_prop_putput(Gen, Val, #lens { s = Put }) ->
    ?FORALL({X, A, A1}, {Gen, Val, Val},
            Put(A1, Put(A, X)) == Put(A1, X)).

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

prop_true() ->
	?FORALL(_X, int(), true).

prop_tuple_1() ->
	% Fix a size of 5
	Sz = 5,
	?FORALL(K, choose(1, Sz),
	    begin
	      lens_prop_getput(g_tuple(Sz), h_tuple(K))
	    end).

prop_tuple_2() ->
	% Fix a size of 5
	Sz = 5,
	?FORALL(K, choose(1, Sz),
	    begin
	      lens_prop_putget(g_tuple(Sz), int(), h_tuple(K))
	    end).

prop_tuple_3() ->
	% Fix a size of 5
	Sz = 5,
	?FORALL(K, choose(1, Sz),
	    begin
	      lens_prop_putput(g_tuple(Sz), int(), h_tuple(K))
	    end).

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
