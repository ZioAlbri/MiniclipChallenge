%%%-------------------------------------------------------------------
%% @doc challenge public API
%% @end
%%%-------------------------------------------------------------------

-module(challenge_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    challenge_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
