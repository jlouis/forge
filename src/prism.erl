-module(prism).

-export([preview/1, review/1]).
-export([p/2, r/2]).

-export([ok/0]).

-record(prism, {
	pre,
	re
}).

preview(#prism { pre = P }) -> P.
review(#prism { re = R }) -> R.

p(#prism { pre = Pre }, Obj) -> Pre(Obj).
r(#prism { re = Re }, Val) -> Re(Val).

ok() ->
    #prism {
      pre = fun
          ({ok, V}) -> V;
          ({error, _}) -> undefined
        end,
      re = fun(V) -> {ok, V} end
    }.
    

      