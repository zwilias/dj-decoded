-module(exercise_05).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% Now that you know how to create a decoder which includes calling a function to transform a
%% primitive decoder, we're all set to take this principle one step further.
%%
%% In this exercise, you'll be feeding 2 fields of an object through a single function to get
%% the response.
%%
%% Concretely; the input will look like this:
%%
%%      { "repeat": 3, "term": "foo" }
%%
%% The expected output is the "term" field, repeated `repeat` times:
%%
%%      "foofoofoo"
%%
%% There is an important lesson to be learned in this exercise - a JS object is essentially an
%% unordered list of key-value pairs. You are free to decode fields in whichever order you like.
%% If you need field `repeat` before you need field `term`; then by all means, decode that field
%% first!

-spec decoder() -> dj:decoder(integer()).
decoder() ->
  dj:mapn( fun binary:copy/2
         , [dj:field(term, dj:binary()), dj:field(repeat, dj:non_neg_integer())]
         ).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_05

decoder_test() ->
  ?assertEqual( {ok, <<"foofoofoo">>}
              , dj:decode(<<"{\"term\":\"foo\",\"repeat\":3}">>, decoder())
              ),
  ?assertEqual({ok, <<"">>}, dj:decode(<<"{\"term\":\"foo\",\"repeat\":0}">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"{\"term\":\"foo\",\"repeat\":-1}">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"\"foobar\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
