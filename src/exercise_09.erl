-module(exercise_09).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% Sometimes - especially when there is no control over the frontend/client or backwards
%% compatibility needs to be preserved - we need to be "flexible" when decoding JSON, yet we
%% still want nicely shaped data to work with.
%%
%% `dj` offers quite a few tools to make dealing with such situations feasible. In this example,
%% we'll take a look at one of the simple building blocks that is used by some of the more
%% advanced solutions.
%%
%% Specifically, we're dealing with JSON where one of the properties can either hold a single
%% value, or a list of values. In case the JSON only has a single value, we still want our
%% result to have a list, so that both can be treated equally in the rest of the code and we
%% only have to deal with the "different" structures at the edge of our system: in our `dj`
%% decoder.
%%
%% Example input #1:
%%
%%     [1, 2, 3]
%%
%% Example output #1:
%%
%%     [1, 2, 3]
%%
%%
%% Example input #2:
%%
%%     123
%%
%% Example output #2:
%%
%%     [123]

-spec decoder() -> dj:decoder([integer()]).
decoder() -> dj:fail(<<"I always fail!">>).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_09

decoder_test() ->
  ?assertEqual({ok, [1, 2, 3]}, dj:decode(<<"[1,2,3]">>, decoder())),
  ?assertEqual({ok, [123]}, dj:decode(<<"123">>, decoder())),
  ?assertEqual({ok, []}, dj:decode(<<"[]">>, decoder())),
  ?assertMatch({error, _}, dj:decode(<<"\"hello\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
