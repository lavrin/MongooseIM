-module(mod_xmpp_dc).
-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([handle_inter_dc_stanza/3]).

-include_lib("exml/include/exml.hrl").
-include("ejabberd.hrl").

start(_Domain, _Opts) ->
    %% TODO: lame! don't register multiple handler instances (i.e. one per vhost)
    DataCenter = get_dc(),
    [ ejabberd_hooks:add(Hook, DataCenter, M, F, Prio)
      || {Hook, M, F, Prio} <- hook_handlers() ],
    ok.

stop(Domain) ->
    DataCenter = get_dc(),
    [ ejabberd_hooks:delete(Hook, DataCenter, M, F, Prio)
      || {Hook, M, F, Prio} <- hook_handlers() ],
    ok.

hook_handlers() ->
    [{local_send_to_resource_hook, ?MODULE, handle_inter_dc_stanza, 50}].

%% Assumptions:
%% - DCFrom and DCTo are data center JIDs, i.e. some-datacenter.some.domain.com/xmpp_dc
%% - actual message to route locally has to be extracted from Packet
handle_inter_dc_stanza(DCFrom, DCTo, #xmlel{name = <<"message">>} = Packet) ->
    ?DEBUG("~p -> ~p: ~p", [DCFrom, DCTo, Packet]),
    [Payload] = [ C || C <- Packet#xmlel.children, element(1, C) == xmlel ],
    From = exml_query:attr(Payload, <<"from">>),
    To = exml_query:attr(Payload, <<"to">>),
    %% assert!
    true = From /= undefined,
    true = To /= undefined,
    %% route it!
    ejabberd_router:route(jlib:binary_to_jid(From),
                          jlib:binary_to_jid(To), Payload),
    ok.

get_dc() ->
    {ok, DataCenter} = application:get_env(mongooseim, datacenter),
    DCJID = jlib:binary_to_jid(DataCenter),
    jlib:jid_to_binary(jlib:jid_remove_resource(DCJID)).
