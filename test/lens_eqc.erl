%%% @doc QuickCheck tests for lenses
%%% @end
-module(lens_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

keys() ->
    elements([a,b,c,d,e,f]).

%% THE THREE PROPERTIES OF LENSES:
lens_props(Gen, Val, L) ->
    View = lens:view(L),
    Set = lens:set(L),
    ?FORALL({X, A, A1}, {Gen, Val,Val},
            conjunction(
              [{putget, equals(A, View(Set(X, A)))},
               {putput, equals(Set(X, A1), Set(Set(X, A), A1))},
               {getput, equals(X, Set(X, View(X)))},
               {transform,
                 ?LET(F, function1(Val),
                     equals(F(View(X)),
                            View(lens:transform(L, X, F))))}])).

prop_id() ->
    ID = lens:id(),
    Gen = int(),
    Val = int(),
    lens_props(Gen, Val, ID).

tuple(K, Gen) ->
    ?LET(V, vector(K, Gen),
         list_to_tuple(V)).

prop_element() ->
    ?FORALL(K, choose(1, 5),
      ?LET(E, choose(1, K),
           begin
               Gen = tuple(K, int()),
               Val = int(),
               lens_props(Gen, Val, lens:element(E))
           end)).

prop_nth() ->
    ?FORALL(K, choose(1,5),
      ?LET(E, choose(1, K),
           begin
               Gen = vector(K, int()),
               Val = int(),
               lens_props(Gen, Val, lens:nth(E))
           end)).

g_map(Key) ->
    ?LET(M, eqc_gen:map(keys(), int()),
         M#{ Key => undefined }).

prop_map() ->
    ?FORALL(Key, keys(),
            begin
                Gen = g_map(Key),
                Val = int(),
                lens_props(Gen, Val, lens:key(Key))
            end).
            
t() ->
    eqc:module({testing_budget, 5}, ?MODULE).
