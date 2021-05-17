-module(exercise_07).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% A small interlude! So far, all of our decoders have meaninfully transformed our input.
%%
%% In this example, however, we'll essentially ignore the input, for the most part[1].
%% The goal is to write a decoder which always succeeds with the atom `foobar`...
%%
%% ^1: the only requirement for a dj decoder to return anything but an error is for the
%%     input string to be well-formed JSON. If it isn't well-formed, the decoder won't run
%%     at all! So, we can ignore that edge-case for the purposes of this experiment :)

-spec decoder() -> dj:decoder(foobar).
decoder() -> dj:fail(<<"I always fail!">>).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_07

decoder_test() ->
  ?assertEqual({ok, foobar}, dj:decode(<<"{\"name\":\"Josh\",\"age\":50}">>, decoder())),
  ?assertEqual({ok, foobar}, dj:decode(<<"[1, 2, 3]">>, decoder())),
  ?assertEqual({ok, foobar}, dj:decode(<<"\"hello\"">>, decoder())),
  ?assertEqual({ok, foobar}, dj:decode(<<"null">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"oops">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
