-module(exercise_02).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% As a second step, let's decode something a tiny bit more complex, that will show you how to
%% compose decoders.
%%
%% Here are two hints: figure out how to decode the _inner_ type, then figure out how to use
%% that to decode what it's enclosed in.
%%
%% Expected input will look like this (as JSON):
%%
%%     [ "foo", "bar", "baz" ];
%%
%% Expected output will look like this in Erlang, on succesfully decoding:
%%
%%     [ <<"foo">>, <<"bar">>, <<"baz">> ]

-spec decoder() -> dj:decoder([binary()]).
decoder() -> dj:fail(<<"I always fail!">>).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_02

decoder_test() ->
  ?assertEqual( {ok, [<<"foo">>, <<"bar">>, <<"baz">>]}
              , dj:decode(<<"[\"foo\",\"bar\",\"baz\"]">>, decoder())
              ),
  ?assertEqual({ok, []}, dj:decode(<<"[]">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"null">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"\"foobar\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
