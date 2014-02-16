-module(trecord).

-include_lib("merl/include/merl.hrl").

-export([parse_transform/2]).

-record(pass1,
	{ t_records = [],
	   generated_functions = false }).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),
            throw({error,get_pos(I),{unknown,R}})
        end).

%% get_pos(I) ->
%%     case proplists:get_value(form, I) of
%% 	undefined ->
%% 	    0;
%% 	Form ->
%% 	    erl_syntax:get_pos(Form)
%%     end.

parse_transform(Forms, Options) ->
	parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
	Acc1 = parse_trans:do_inspect(fun inspect_f/4, #pass1{}, Forms, Context),
	{NewForms, _Acc2} = parse_trans:do_transform(fun generate_f/4, Acc1, Forms, Context),
	parse_trans:revert(NewForms).
	
inspect_f(attribute, {attribute, _L, trecord, RecDef}, _Ctx, Acc) ->
	{false, Acc#pass1 { t_records = [RecDef | Acc#pass1.t_records ]}};
inspect_f(_Type, _Form, _Context, Acc) ->
	{false, Acc}.
	
generate_f(attribute, {attribute, _L, record, _RecDef} = Form, _Ctx, Acc) ->
	{Form, false, Acc};
generate_f(attribute, {attribute, L, trecord, RecDef}, _Ctx, Acc) ->
	Exports = generate_exports_for_trecord(RecDef),
	{[], generate_record(L, RecDef), [{attribute, L, export, Exports}], false, Acc};
generate_f(function, Form, _Context, #pass1 { generated_functions = false } = Acc) ->
	L = erl_syntax:get_pos(Form),
	Forms = generate_map_converters(L, Acc),
	{Forms, Form, [], false, Acc#pass1 { generated_functions = true }};
generate_f(_Type, Form, _Context, Acc) ->
	{Form, false, Acc}.

generate_map_converters(L, #pass1 { t_records = Defs } ) ->
	[generate_mc_def(L, D) || D <- Defs].
	
generate_mc_def(L, {Name, Attrs}) ->
	{function, L, list_to_atom(atom_to_list(Name) ++ "_to_jsx"), 1,
		[{clause, L,
			[generate_record_pattern(L, Name, Attrs, 0)],
			[],
			[generate_mapping_list(L, Name, Attrs, 0)]
		}]
	}.
	
gensym(X) ->
	list_to_atom("X_" ++ integer_to_list(X)).

generate_record_pattern(L, Name, Attrs, SymbC) ->
	{_, Pats} = lists:foldl(
		fun({AN, _}, {SC, Acc}) ->
			G = gensym(SC),
			A = {record_field, L, {atom, L, AN}, {var, L, G}},
			{SC+1, [A | Acc]}
		end,
		{SymbC, []},
		Attrs),
	{record, L, Name, Pats}.

generate_mapping_list(L, Name, [{K, _Tags} | Next], SymbC) ->
	G = {var, L, gensym(SymbC)},
	T = ?Q(
		["case _@G of",
		 "  undefined -> null;",
		 "  V -> V",
		 "end"]),
	Tup = {tuple,15,[{atom,15,K},erl_syntax:revert(T)]},
	{cons, L, Tup,
		generate_mapping_list(L, Name, Next, SymbC+1)};
generate_mapping_list(L, _Name, [], _SymbC) ->
	{nil, L}.

generate_exports_for_trecord({RecName, _}) ->
	[{fname(RecName, '_to_jsx'), 1}].

generate_record(L, {RecName, RecAttribs}) ->
	NewDef = {RecName, generate_record_fields(L, RecAttribs)},
	{attribute, L, record, NewDef}.

%% Helper until maps get more powerful :)
mg(K, M) ->
    try
    	{value, maps:get(K, M)}
    catch
    	error:bad_key -> not_found
    end.

generate_record_fields(_L, []) -> [];
generate_record_fields(L, [{ Name, M } | Next]) when is_atom(Name) ->
	F = case mg(default, M) of
		not_found -> {record_field, L, {atom, L, Name}};
		{value, DefT} ->
			Def = ?Q("_@DefT@"),
			{record_field, L, {atom, L, Name}, erl_syntax:revert(Def)}
	end,
	[F | generate_record_fields(L, Next)].

%% rpt_error(Reason, Fun, Info) ->
%%     Fmt = lists:flatten(
%% 	    ["*** ERROR in parse_transform function:~n"
%% 	     "*** Reason     = ~p~n",
%%              "*** Location: ~p~n",
%% 	     ["*** ~10w = ~p~n" || _ <- Info]]),
%%     Args = [Reason, Fun |
%% 	    lists:foldr(
%% 	      fun({K,V}, Acc) ->
%% 		      [K, V | Acc]
%% 	      end, [], Info)],
%%     io:format(Fmt, Args).

fname(Prefix, Op) ->
	list_to_atom(atom_to_list(Prefix) ++ atom_to_list(Op)).
