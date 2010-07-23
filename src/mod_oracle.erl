-module(mod_oracle).

-behaviour(gen_module).

-export([execute/2]).

%% gen_module callbacks
-export([start/1, stop/0]).

-include("netspire.hrl").

start(Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    case start_oracle_srv(Options) of
        {ok, _Pid} ->
            application:set_env(netspire, database_backend, oracle);
        _Any ->
            ?ERROR_MSG("Cannot start ~p due to ~p~n", [?MODULE, _Any])
    end.

start_oracle_srv(Options) ->
    ChildSpec = {netspire_oracle_srv,
        {netspire_oracle_srv, start_link, [Options]},
        transient,
        3000,
        worker,
        [netspire_oracle_srv]
    },
    supervisor:start_child(netspire_sup, ChildSpec).

stop() ->
    ?INFO_MSG("Stopping dynamic module ~p~n", [?MODULE]),
    application:unset_env(netspire, database_backend),
    supervisor:terminate_child(netspire_sup, netspire_oracle_srv),
    supervisor:delete_child(netspire_sup, netspire_oracle_srv).

execute(Q, Param) ->
    netspire_oracle_srv:equery(Q, Param).
