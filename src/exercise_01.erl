-module(exercise_01).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% So, you're mystified by JSON decoding with DJ? No worries, let's try and make them *click*
%% for you, by going through a set of exercises where you'll be gradually creating more and more
%% complex JSON decoders.
%%
%% So what _is_ a JSON decoder, really? Essentially, it is a way to go from a raw JSON
%% (represented as a `binary()`) to nice, neat Erlang types, where you fully control how the
%% values are transformed and which kinds of values are allowed.
%%
%% The `dj` library gives you a whole function functions that generally fall into 4-ish categories:
%%
%% - decoders for dealing with primitive types and structures
%% - combinators to combine and manipulate decoders
%% - a few "higher level" decoders (for example `dj:uuid/1`)
%% - functions to "run" a decoder over some input
%%
%% So, have a little look at the docs and check out some of the decoders for dealing with
%% primitive values. After reading through those, you should be able to implement the below
%% decoder, so that it can handle json values like `5` and `-20`!

-spec decoder() -> dj:decoder(integer()).
decoder() -> dj:integer().

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_01

decoder_test() ->
  ?assertEqual({ok, 5}, dj:decode(<<"5">>, decoder())),
  ?assertEqual({ok, -20}, dj:decode(<<"-20">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"1.2">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"\"foobar\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
