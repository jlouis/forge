-module(prism).

-export([pre/2, re/2]).

-export([ok/0]).

-record(prism, {
	pre,
	re
}).

pre(#prism { pre = Pre }, Obj) -> Pre(Obj).
re(#prism { re = Re }, Val) -> Re(Val).

ok() ->
    #prism {
      pre = fun
          ({ok, V}) -> V;
          ({error, _}) -> undefined
        end,
      re = fun(V) -> {ok, V} end
    }.
    

      