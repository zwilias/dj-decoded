-module(exercise_03).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% So, this one is a tiny bit more challenging, since we don't want _just_ the contents of the
%% JSON. No, we want them to be modified.
%%
%% input:
%%       [ "foo", "bar" ];
%%
%% output:
%%       [ <<"FOO">>, <<"BAR">> ]

-spec decoder() -> dj:decoder([binary()]).
decoder() -> dj:fail(<<"I always fail!">>).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_03

decoder_test() ->
  ?assertEqual({ok, [<<"FOO">>, <<"BAR">>]}, dj:decode(<<"[\"foo\",\"bar\"]">>, decoder())),
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
