%% -*- coding: utf-8 -*-
%% module lens implements functional lenses for erlang
-module(lens).

%% Creating lenses
-export([make/2]).

%% View/Set
-export([v/2, s/3]).
-export([view/1, set/1]).

%% Transformation
-export([transform/3]).

%% Composition
-export([compose/1, compose/2]).

%% Joins
-export([join_list/1]).

%% Standard Lenses
-export([
         element/1,
         nth/1,
         key/1,
         id/0
]).

-record(lens, {
          viewer,
          setter
}).

make(Viewer, Setter) ->
    #lens { viewer = Viewer, setter = Setter }.

view(#lens { viewer = V }) -> V.
set(#lens { setter = S }) -> S.

v(#lens { viewer = V }, T) -> V(T).
s(#lens { setter = S }, T, Val) -> S(T, Val).

transform(#lens { viewer = V, setter = S}, D, F) ->
    S(D, F(V(D))).

compose(#lens { viewer = V1, setter = S1}, #lens { viewer = V2, setter = S2}) ->
    #lens {
       viewer = fun(X) -> V2(V1(X)) end,
       setter = fun(T, Value) ->
                        SubT = V1(T),
                        NSubV = S2(SubT, Value),
                        S1(T, NSubV)
                end
      }.

compose(Args) ->
    lists:foldr(fun compose/2, id(), Args).

%% -- JOINS/LIST ------------------------------------
join_list(Lenses) ->
    #lens {
       viewer = fun(T) -> [(view(L))(T) || L <- Lenses] end,
       setter = fun(T, Vs) -> join_list_set(T, Vs, Lenses) end
      }.

join_list_set(T, [], []) -> T;
join_list_set(T, [V|Vs], [L|Ls]) ->
    NewT = (set(L))(T, V),
    join_list_set(NewT, Vs, Ls).
                        
%% -- BASE LENSES ----------------------------------
id() ->
    #lens {
       viewer = fun(X) -> X end,
       setter = fun(_, X) -> X end
      }.

%% -- LENSES ON TUPLES ---------------------------
element(N) ->                    
    #lens {
       viewer = fun(Tup) -> element(N, Tup) end,
       setter = fun(Tup, Val) -> setelement(N, Tup, Val) end
      }.

%% -- LENSES ON LISTS -----------------------------------
nth(N) ->
    #lens {
       viewer = fun(L) -> lists:nth(N, L) end,
       setter = fun(L, V) ->
                        {First, [_|Tail]} = lists:split(N-1, L),
                        First ++ [V|Tail]
                end
       }.

%% -- LENSES ON MAPS ------------------------------------
key(Key) ->
    #lens {
       viewer = fun(M) -> maps:get(Key, M) end,
       setter = fun(M, V) -> maps:put(Key, V, M) end
      }.

                        
                        
                        
                         
