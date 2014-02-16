-module(trecord_c).

-include_lib("merl/include/merl.hrl").

-export([compile/2, t/0]).

-type props() :: {json_key, atom() | binary()} | {default, any()} | {type, string()}.
-type trecord() :: {atom(), [{atom(), [props()]}]}.
  
t() ->
	compile([{foo, [{a, [{json_tag, "fooQuux"}]}, {b, [{default, 42}]} ]}], bar).

-spec compile( trecord(), atom() ) -> any().
compile(TRecords, ModName) ->
	Forms = merl_build:module_forms(
		lists:foldl(
			fun ({X, Name, Cs}, S) ->
				merl_build:add_function(X, Name, Cs, S)
			end,
			init_module(ModName, TRecords),
			[{true, to_json, [to_json(hd(TRecords))]}])),
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
	JSONTag = case proplists:get_value(json_tag, Props, undefined) of
		undefined -> F;
		Tag when is_list(Tag) -> Tag
	end,
	JSONExpr = ?Q(["{_@JSONTag@, element(_@S@, Rec)}"]),
	[JSONExpr | to_json_ids(Name, Ids, S+1)].

gensym(I) ->
	merl:term(
		list_to_atom("Z" ++ integer_to_list(I))).
