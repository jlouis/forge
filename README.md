# Project Forge

The `forge` package provides ways to *melt* and *recast* Erlang data. That is, it provides a way to help Erlang programmers take data and restructure it so it matches what they need.

The current implementations for doing this are:

* `lens` : A library for non-failing functional lenses

# Lens Tutorial

Lenses work a bit like the `sofs` library for Erlang: once you know how to use it, you can express thing that were previously hard to do in Erlang. Lenses solve the problem of working with deep nested data structures without picking them apart all the time. Rather, you use lenses to define what you want to change, and then you apply those lenses to change the structure. It gives you the equivalent of a pointed update in imperative languages:

	struct.a.b.c = 3

but in a pure functional setting.

Lenses are useful in the following situations:

* You often manipulate specific data in a deeply nested structure.
* You need to manipulate many sub-parts of a deeply nested structure at the same time.
* You end up writing many functions pattern matching their way to the target element.

Their use is not on data, where you have no need to go deep into the nesting. A simple pattern match tend to be enough to pick apart the data structure you are working on.

Here is the internal `lens` record:

	#lens { viewer, setter }
	
It contains two functions, the *view* and the *set* function. They work together with an object, normally called the *target* of the lens. Here is the mnemonic idea: the lens provides a optical way to operate on the target. You can *view* the target through the lens, which focuses on a sub-part of the target. And you can *set* that subpart through the lens, which updates that part only, but keeps the rest of the *target* unchanged.

**Example:** If we have a tuple `{a, 10}`, we can define a view on the 1st element of that tuple: `V = fun(T) -> element(1, T) end`. And we can set that element as well to a new value with a function `S = fun(T, NewVal) -> setelement(1, T, NewVal) end`. Together these form a lens. This lens is also available by calling the function `lens:element(1)`. We can use the lens like follows:

	1> L = lens:element(1),
	2> lens:v(L, {a, 10}).
	a

The `v/2` function uses the view of the lens. Similarly, we can use the `s/3` function for setting things via the lens:

	3> lens:s(L, {a, 10}, b).
	{b,10}

## Lens Rules

The lens you saw has a peculiar relation between the view and the set function. These are codified in the *lens rules*. All lenses you define must obey the rules, and it is up to you to check that they do. We assume a specific lens with functions `view` and `set` here:

* [PutGet] for any target T and value A: `A = view(set(T, A))`.
* [GetPut] for any target T: `T = set(T, view(T))`.
* [PutPut] for any target T and values A, A1: `set(T, A1) = set(set(T, A), A1)`

These rules state that the lens behaves "as it ought to" when operating on data. In particular, it establishes a relation between the view and the set function of the lens. The rules are important because they make sure the lenses we define are well-behaved, even if we start to combine them.

## Combining lenses

The power of lenses are that they can be combined, or composed into larger lenses. This is what makes them effective for updating complex data structures, by picking the structure apart and joining it together again. Like in a camera, we can combine several lenses in order to glean inside more complex structure:

	4> L2 = lens:element(2).
	5> L3 = lens:compose(L, L2).
	6> lens:v(L3, {{x, y}, {3, 5}}).
	y

The composition says: "run L and then run L2 on the result". So we pick the first element, and then we pick the second element of the result, obtaining `y`. But we can also use the lens to *set* that element:

	7> lens:s(L3, {{x, y}, {3, 5}}, z).
	{{x,z},{3,5}}

which is the power of the lens-abstraction: it allows us to dig "deep" inside structure and update data deeply.

## Lenses on Tuples / Records

We've already met the tuple lens: `lens:element(K)` for a given `K`. This lens can also work on records, as

	RL = lens:element(#rec.a),
	
will define a lens of the record value of `a`. I.e., given a record `R`, we have:

	R#rec.a = lens:v(RL, R)
	
## Lenses on Lists

On lists, lenses introduces a new way of operating. There is a lens, `lens:nth(K)` for a `K` which operates on the `K`th element of a list. But this lens is often rather slow to use for large lists, so use it with caution.

But note that there is a *join* over lists with lenses as well. In a join, we combine several lenses into a view of the structure:

	8>
	  E1 = lens:element(1),
	  E2 = lens:element(3),
	  T = {1,2,3,4},
	  JL = lens:join_list([E1, E2]),
	  lens:v(JL, T).
	
	[1,3]

And of course, we can use the join to set elements as well:

	9> lens:s(JL, T, [a, b])
	{a,2,b,4}

## Lenses on Maps

On maps, you have the lens `lens:key(Key)` where `Key` is the map key to focus on. It works just like every other lens:

	9> L = lens:key(a),
	10> lens:v(L, #{ a => 3, 10 => 20 }).
	3
	11> lens:s(L, #{ a => 3, 10 => 20 }, xyzzy).
	#{10 => 20,a => xyzzy}

There is also a join variant on hashes:

	> T = {1,2,3,4},
	> E1 = lens:element(1).
	> E2 = lens:element(2).
	> JL = lens:join_map(#{ a => E1, b => E2}).
	> lens:v(JL, T).
	#{ a := 1, b := 2}
	
Where the set function works in the obvious way:

	> lens:s(JL, T, #{ a => x, b => y}).
	{x,y,3,4}


# Prism tutorial

Lenses work on things which are "products" in nature. That is, they work on objects which is a X *and* an Y *and* a Z, …, and so forth. The dual notion is that of a prism, which works on things which are A *or* B *or* … in nature. In other words, they operate over sum-types. A sum which is commonly used in Erlang is the computation with failure:

	{ok, Term} | {error, Reason}
	
We can define a prism for this structure. A prism is given by a pair of functions, just like the lens

	#prism { preview, review }
	
where the types of the preview and review are "inverted". The function `prism:pre(P, Obj) -> {ok, V} | undefined` defines the preview function of a prism. Given a prism, we can "focus" inside an object to obtain either a value, or undefined. We have the prism `prism:ok()` which focuses on the `ok` part of the above error type. So we have:

	12> P = prism:ok().
	13> prism:pre(P, {ok, xyzzy}).
	xyzzy
	14> prism:pre(P, {error, not_found}).
	undefined
	
Likewise, the function `prism:re(P, V) -> Obj`, will hoist our value inside an object by wrapping it into a result. For our *ok-prism* this correspond to wrapping a value into an ok-tuple

	15> prism:re(P, abracadabra),
	{ok, abracadabra}

## Prism laws

Like the lenses, prisms must support certain laws for them to work. In particular, they must support relations between their *preview* and *review* functions. The first one is the following rule:

	preview (review A) = {ok, A}

it states that there is an identity between wrapping a value `A` and then unwrapping it again. Furthermore, there is a rule in the other direction:

	preview X = {ok, A} ===> (implies)
		review A = X
		
That is, if we have a prism which can succesfully focus on a value `A`, then reviewing that value reconstructs the original structure *exactly*. That is, there is no other structure in a prism.

# Traversals

…

# QuickCheck

QuickChecked libraries:

* `lens` - Almost every function under QuickCheck, but the model still needs some specification to become entirely stable. In particular, I want to generate a lens together with a generator for an underlying object for that lens. This allows one to test the deeply nested lens structure easily by invoking the nested generators as we go along.

