-module(exercise_04).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% Something very powerful about decoding using `dj`, is that it decouples the internal
%% representation of your data from the external representation. In this exercise, we'll explore
%% that, _very very_ briefly: we'll make a decoder that extracts a single piece of information
%% from an object.
%%
%% input:
%%
%%      { "age": 50 }
%%
%% output:
%%
%%      50
%%
%% In order to to this, you'll need to run a primitive decoder on the value of that field. How
%% can we do this? Well, the `dj` happens to expose a number of decoders that allow "navigating"
%% JSON objects, and running a decoder on their "target". In this case, you know that the field
%% is named `age`, so you'll probably need something that can take the name of a field, a
%% decoder, and that will apply that decoder to the given field!

-spec decoder() -> dj:decoder(integer()).
decoder() -> dj:fail(<<"I always fail!">>).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_04

decoder_test() ->
  ?assertEqual({ok, 50}, dj:decode(<<"{\"age\":50}">>, decoder())),
  ?assertEqual({ok, 123}, dj:decode(<<"{\"age\":123}">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"{}">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"\"foobar\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
