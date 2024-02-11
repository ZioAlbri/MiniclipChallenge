%%%-------------------------------------------------------------------
%% @doc challenge public API
%% @end
%%%-------------------------------------------------------------------

-module(challenge_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    challenge_sup:start_link(),
    server:start(),
    ok.

stop(_State) ->
    ok.
