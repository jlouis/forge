-module(trecord_test).

-compile({parse_transform, trecord}).

-export([x/0, y/0]).

-record(bar,
	{baz = 3, quux}
).

-trecord({foo, 
	[{a, #{ default => 1, json => "a_tag" }},
	 {b, #{ json => "b_tag" }}
]}).
	
x() ->
	Foo = #foo{},
	Bar = #bar{},
	io:format("~p~n", [[{foo, Foo}, {bar, Bar}]]).

y() ->
	Foo = #foo{},
	foo_to_jsx(Foo).
