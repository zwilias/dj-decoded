-module(exercise_10).

-include_lib("eunit/include/eunit.hrl").

-export([decoder/0]).

%% Bonus round: a more complex example!
%%
%% I can't quite come up with a good example for when this type of decoder would be needed.
%% However, programmers are strange creatures and their code sometimes produces very ...
%% interesting JSON!
%%
%% In this case, we have a level of indirection within our JSON. Since an example here might be
%% more clear than trying to describe things, let's do exactly that:
%%
%% Example input:
%%
%%     {
%%       "keys": ["key3", "key1", "key2"],
%%       "data": {
%%         "key1": "to the",
%%         "key2": "world!",
%%         "key3": "Hello"
%%       }
%%     }
%%
%% Example output:
%%
%%     [<<"Hello">>, <<"to the">>, <<"world!">>]
%%
%%
%% So, we want to return the values of the fields identified in `"keys"`, as a list, in the same
%% order as defined by the `"keys"` field... Good luck!

-spec decoder() -> dj:decoder([integer()]).
decoder() -> dj:fail(<<"I always fail!">>).

%% Tests
%%
%% To run these, try:
%%
%%   rebar3 eunit --module=exercise_10

decoder_test() ->
  ?assertEqual( {ok, [<<"Hello">>, <<"to the">>, <<"world!">>]}
              , dj:decode( << "{"
                              "\"keys\":[\"key3\", \"key1\", \"key2\"],"
                              "\"data\":{"
                              "\"key1\":\"to the\","
                              "\"key2\":\"world!\","
                              "\"key3\":\"Hello\""
                              "}"
                              "}"
                           >>
                         , decoder()
                         )
              ),
  ?assertEqual( {ok, [<<"Hello">>, <<"world!">>]}
              , dj:decode( << "{"
                              "\"keys\":[\"baz\", \"bar\"],"
                              "\"data\":{"
                              "\"baz\":\"Hello\","
                              "\"key2\":null,"
                              "\"bar\":\"world!\""
                              "}"
                              "}"
                           >>
                         , decoder()
                         )
              ),
  ?assertMatch({error, _}, dj:decode(<<"\"hello\"">>, decoder())).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
