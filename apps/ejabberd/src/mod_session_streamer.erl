-module(mod_session_streamer).
-behaviour(gen_server).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% gen_server callbacks
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Hook handlers
-export([session_opened/3,
         session_closed/4]).

%% temp
-compile([export_all]).

-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_session_streamer).
-define(EVENT_TYPE, {table, session, simple}).

-record(state, {sink, socket}).

%%
%% gen_mod callbacks
%%

start(Domain, Opts) ->
    %% make sure only one copy is running!
    {ok, _Pid} = start_streamer(Opts),
    [ ejabberd_hooks:add(Hook, Domain, M, F, Prio)
      || {Hook, M, F, Prio} <- hook_handlers() ],
    ok.

stop(Domain) ->
    [ ejabberd_hooks:delete(Hook, Domain, M, F, Prio)
      || {Hook, M, F, Prio} <- hook_handlers() ],
    catch supervisor:terminate_child(ejabberd_sup, ?PROCNAME),
    catch supervisor:delete_child(ejabberd_sup, ?PROCNAME),
    ok.

%%
%% Hook handlers
%%

hook_handlers() ->
    [{sm_register_connection_hook, ?MODULE, session_opened, 25},
     {sm_remove_connection_hook, ?MODULE, session_closed, 25}].

session_opened(_SID, JID, _Info) ->
    catch gen_server:cast(?PROCNAME, {session_opened, JID}),
    ok.

session_closed(_SID, JID, _Info, _Reason) ->
    catch gen_server:cast(?PROCNAME, {session_closed, JID}),
    ok.

%%
%% gen_server callbacks
%%

start_link(Opts) ->
    gen_server:start_link({local, ?PROCNAME}, ?MODULE, Opts, []).

init(Opts) ->
    {Address, Port} = get_sink(default_opts(Opts)),
    self() ! reconnect,
    {ok, #state{sink = {Address, Port}}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({session_opened, JID} = Ev, #state{} = S) ->
    ?INFO_MSG("Session for ~ts opened", [jlib:jid_to_binary(JID)]),
    send_event(S#state.socket, Ev),
    {noreply, S};
handle_cast({session_closed, JID} = Ev, #state{} = S) ->
    ?INFO_MSG("Session for ~ts closed", [jlib:jid_to_binary(JID)]),
    send_event(S#state.socket, Ev),
    {noreply, S}.

handle_info(reconnect, #state{socket = undefined} = S) ->
    {noreply, reconnect(S)};
handle_info(Info, #state{} = S) ->
    ?INFO_MSG("Unexpected info: ~p, ~p", [Info, S]),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%

start_streamer(Opts) ->
    case supervisor:start_child(ejabberd_sup, spec(Opts)) of
        {ok, Pid} ->
            {ok, Pid};
        {error, already_present} ->
            %% TODO: maybe just restart and proceed?
            ?ERROR_MSG("already_present: should be running but is not", []),
            error(already_present, [spec_present_but_child_not_running]);
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

spec(Opts) ->
    {?PROCNAME,
     {?MODULE, start_link, [Opts]},
     permanent, timer:seconds(2), worker, [?MODULE]}.

get_sink(Opts) ->
    {sink, Sink} = lists:keyfind(sink, 1, Opts),
    Sink.

default_opts(Opts) ->
    (default(sink, {{127,0,0,1}, 3334}, Opts) ++
     Opts).

default(Opt, Value, Opts) ->
    [ {Opt, Value} || not proplists:is_defined(Opt, Opts) ].

reconnect(#state{sink = {Address, Port}} = S) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {packet, 2}]),
    S#state{socket = Socket}.

send_event(Socket, {SessionEvent, JID}) ->
    {ok, DataCenter} = application:get_env(mongooseim, datacenter),
    Ev = {SessionEvent, DataCenter, JID},
    try gen_tcp:send(Socket, term_to_binary(Ev)) of
        ok -> ok
    catch
        _:_ -> self() ! reconnect
    end.
