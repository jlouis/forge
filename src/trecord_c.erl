-module(trecord_c).

-include_lib("merl/include/merl.hrl").

-export([compile/2, t/0]).

-type props() :: {json_key, atom() | binary()} | {default, any()} | {type, string()}.
-type trecord() :: {atom(), [{atom(), [props()]}]}.
  
t() ->
	compile([
		{qux, [{a, [{json_tag, "zot"}, {default, "Hello"}]}, {b, []}] },
		{foo, [{a, [{json_tag, "fooQuux"}, {default, 42}]}, {b, [{rec, qux}]} ]}
		],
		mod).

-spec compile( trecord(), atom() ) -> any().
compile(TRecords, ModName) ->
	Forms = merl_build:module_forms(
		lists:foldl(
			fun ({X, Name, Cs}, S) ->
				merl_build:add_function(X, Name, Cs, S)
			end,
			init_module(ModName, TRecords),
			[{true, to_json, [to_json(TR) || TR <- TRecords]},
			 {true, from_json, [from_json(TR) || TR <- TRecords]}]
	)),
	file:write_file(lists:concat([ModName, "_gen.erl"]),
		erl_prettypr:format(erl_syntax:form_list(Forms),
			[{paper, 160}, {ribbon, 80}]) ).

init_module(ModName, Records) ->
	M = merl_build:init_module(ModName),
	lists:foldl(
		fun(Rec, Mod) ->
		  {Name, Fields} = c_record(Rec),
		  merl_build:add_record(Name, Fields, Mod)
		end,
		M,
		Records).

c_record({Name, Ids}) ->
	B = fun(F, Props) -> {F, merl:term(proplists:get_value(default, Props, undefined))} end,
	Fields = [ B(F, Props) || {F, Props} <- Ids],
	{Name, Fields}.

to_json({Name, Ids}) ->
	JSXExprs = to_json_ids(Name, Ids, 1),
	?Q(["(Rec) when is_record(Rec, _@Name@) -> [_@JSXExprs]"]).

to_json_ids(_Name, [], _) -> [];
to_json_ids(Name, [{F, Props} | Ids], S) ->
	JSONTag = tag(F, Props),
	JSONExpr = case rec(Props) of
		undefined -> ?Q(["{_@JSONTag@, element(_@S@, Rec)}"]);
		{rec, _} -> ?Q(["{_@JSONTag@, to_json(element(_@S@, Rec))}"])
	end,
	[JSONExpr | to_json_ids(Name, Ids, S+1)].

from_json({Name, Ids}) ->
	Exprs = from_json_ids(Name, Ids, 1),
	?Q([
	  "(JSON, Rec) when is_record(Rec, _@Name@) ->"
	  "    X0 = Rec,"
	  "	   _@Exprs"]).
	
from_json_ids(_Name, [], S) ->
	G = gensym(S),
	?Q(["_@G"]);
from_json_ids(Name, [{F, Props} | Ids], S) ->
	Next = from_json_ids(Name, Ids, S+1),
	JSONTag = tag(F, Props),
	case rec(Props) of
		undefined ->
			Before = gensym(S-1),
			Sym = gensym(S),
			?Q(["_@Sym = setelement(_@S@, _@Before, proplists:get_value(_@JSONTag@, JSON, undefined)), _@Next"]);
		{rec, ObjRec} ->
			Before = gensym(S-1),
			Sym = gensym(S),
			?Q(["_@Sym = setelement(_@S@, _@Before, from_json(proplists:get_value(_@JSONTag@, JSON, undefined), _@ObjRec@)), _@Next"])
	end.
	
rec(Props) ->
	case proplists:get_value(rec, Props, undefined) of
		undefined -> undefined;
		R -> {rec, R}
	end.

tag(F, Props) ->
	case proplists:get_value(json_tag, Props, undefined) of
		undefined -> F;
		Tag when is_list(Tag) -> Tag
	end.

gensym(S) ->
	Var = list_to_atom("X" ++ integer_to_list(S)),
	merl:var(Var).
