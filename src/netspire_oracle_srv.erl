-module(netspire_oracle_srv).

-behaviour(gen_server).

-export([start_link/1, equery/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).

-include("netspire.hrl").

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

equery(Q, Params) ->
    gen_server:call(?MODULE, {equery, Q, Params}).

init(Options) ->
    process_flag(trap_exit, true),
    [UserName, Password, SID] = Options,
    case netspire_oracle_drv:init() of
        ok ->
            ?INFO_MSG("Oracle driver is loaded~n", []),
            case netspire_oracle_drv:connect(UserName, Password, SID) of
                ok ->
                    ?INFO_MSG("Oracle successful connected to database~n", []);
                {error, {_Code, Reason}} ->
                    ?ERROR_MSG("Cannot connect to database due to ~s~n", [Reason])
            end;
        {error, {_, Reason}} ->
            ?ERROR_MSG("~s~n", [Reason])
    end,
    {ok, []}.

handle_call({equery, Q, Params}, _From, State) ->
    Reply = netspire_oracle_drv:equery(Q, Params),
    {reply, Reply, State};

handle_call(_, _, State) -> {noreply, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    case netspire_oracle_drv:is_connected() of
        true ->
            ?INFO_MSG("Disconnecting from oracle database~n", []),
            netspire_oracle_drv:disconnect();
        false -> ok
    end.

