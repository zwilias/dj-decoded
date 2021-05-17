-module(exercise_08).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0, all_colors/0]).

%% Let's talk about atoms! Atoms are often used to represent "well known atomic values". One
%% use-case where they often show up, is when dealing with enum-like types.
%%
%% For example, in this exercise, we'll be dealing with the `color/0` type as defined below. For
%% convenience, a function returning a list of valid atoms for this type is also provided.
%%
%% The goal is to decode the input string into a valid color, or fail when the input does not
%% correspond to a valid color.
%%
%% Example input:
%%
%%     "blue"
%%
%% Example output:
%%
%%     blue

-type color() :: red | green | blue.

-spec all_colors() -> [color()].
all_colors() -> [red, green, blue].

-spec decoder() -> dj:decoder(color()).
decoder() -> dj:atom(all_colors()).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_08

decoder_test() ->
  ?assertEqual({ok, red}, dj:decode(<<"\"red\"">>, decoder())),
  ?assertEqual({ok, green}, dj:decode(<<"\"green\"">>, decoder())),
  ?assertEqual({ok, blue}, dj:decode(<<"\"blue\"">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"\"error\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
