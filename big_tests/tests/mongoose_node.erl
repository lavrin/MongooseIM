-module(mongoose_node).

-export([require/1,
         from_config/1,
         rpc/4,
         cwd/1]).

-type t() :: #{node := node(),
               domain := binary(),
               atom() => any()}.

-spec require([atom()]) -> [{require, any()}].
require(Handles) when is_list(Handles) ->
    [ {require, {hosts, Handle}} || Handle <- Handles ].

-spec from_config(atom()) -> t().
from_config(Handle) when is_atom(Handle) ->
    maps:from_list(ct:get_config({hosts, Handle})).

-spec rpc(t(), _, _, _) -> any().
rpc(Node, M, F, A) ->
    Spec = maps:with_keys([node, cookie, timeout], Node),
    distributed_helper:rpc(Spec, M, F, A).

-spec cwd(t()) -> string().
cwd(Node) ->
    {ok, CWD} = rpc(Node, file, get_cwd, []),
    CWD.
