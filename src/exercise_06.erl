-module(exercise_06).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% A common use-case for decoders is decoding objects into nicely typed maps.
%%
%% In this example, we'll do a fairly straightforward conversion of a JSON object
%% to a map, where we want to validate that we only "extract" the required fields,
%% and that the values conform to our expectations.
%%
%% Example input:
%%
%%     {"name": "Josh", "age": 50}
%%
%% Example output:
%%
%%     #{name => <<"Josh">>, age => 50}
%%
%% Make sure that the result only have `name` and `age`, with the fields being a binary
%% and a non-negative integer!

-spec decoder() -> dj:decoder(#{name := binary(), age := non_neg_integer()}).
decoder() ->
  dj:to_map(#{name => dj:field(name, dj:binary()), age => dj:field(age, dj:non_neg_integer())}).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_06

decoder_test() ->
  ?assertEqual( {ok, #{name => <<"Josh">>, age => 50}}
              , dj:decode(<<"{\"name\":\"Josh\",\"age\":50}">>, decoder())
              ),
  ?assertEqual( {ok, #{name => <<"Newborn">>, age => 0}}
              , dj:decode(<<"{\"age\":0,\"name\":\"Newborn\",\"foo\":\"bar\"}">>, decoder())
              ),
  ?assertMatch({error, _}, dj:decode(<<"{}">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"{\"name\":null,\"age\":5}">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"{\"name\":\"foo\",\"age\":-5}">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
