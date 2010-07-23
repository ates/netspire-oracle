-module(netspire_oracle_drv).

-export([init/0, connect/3, disconnect/0, equery/1, equery/2, is_connected/0]).

-define(nif_stub, nif_stub_error(?LINE)).

init() ->
    PrivDir =
        case code:priv_dir(netspire) of
            {error, bad_name} -> "./priv";
            D -> D
        end,
    LibDir = filename:join([PrivDir, "lib", ?MODULE]),
    erlang:load_nif(LibDir, 1).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

connect(_UserName, _Password, _ConnStr) -> ?nif_stub.
disconnect() -> ?nif_stub.
is_connected() -> ?nif_stub.
equery(_Q) -> ?nif_stub.

equery(Query, Params) ->
    equery(lists:flatten(io_lib:format(Query, Params))).

