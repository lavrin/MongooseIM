-module(mod_session_sink).
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

%% temp
-compile([export_all]).

-include("jlib.hrl").
-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_session_sink).

-record(state, {listen_on, listen_socket}).
-record(remote_session, {bare_jid, datacenter}).

%%
%% gen_mod callbacks
%%

start(Domain, Opts) ->
    mnesia:create_table(remote_session, [{ram_copies, [node()]},
                                         {attributes, record_info(fields, remote_session)}]),
    mnesia:add_table_copy(remote_session, node(), ram_copies),
    %% make sure only one copy is running!
    {ok, _Pid} = start_listener(Opts),
    ok.

stop(Domain) ->
    catch supervisor:terminate_child(ejabberd_sup, ?PROCNAME),
    catch supervisor:delete_child(ejabberd_sup, ?PROCNAME),
    ok.

%%
%% gen_server callbacks
%%

start_link(Opts) ->
    gen_server:start_link({local, ?PROCNAME}, ?MODULE, Opts, []).

init(Opts) ->
    self() ! listen,
    {ok, #state{listen_on = get_listen_address(default_opts(Opts))}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Cast, #state{} = S) ->
    {noreply, S}.

handle_info(listen, #state{listen_socket = undefined} = S) ->
    {noreply, listen(S)};
handle_info(accept, #state{} = S) ->
    {noreply, accept(S)};
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

start_listener(Opts) ->
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

get_listen_address(Opts) ->
    {listen_on, ListenOn} = lists:keyfind(listen_on, 1, Opts),
    ListenOn.

default_opts(Opts) ->
    (default(listen_on, 3334, Opts) ++
     Opts).

default(Opt, Value, Opts) ->
    [ {Opt, Value} || not proplists:is_defined(Opt, Opts) ].

listen(#state{listen_on = Port} = S) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {packet, 2}, {active, once},
                                         {reuseaddr, true}]),
    self() ! accept,
    S#state{listen_socket = Socket}.

accept(#state{listen_socket = ListenSocket} = S) ->
    case gen_tcp:accept(ListenSocket, timer:seconds(10)) of
        {ok, Socket} ->
            {ok, _} = start_handler(Socket);
        {error, timeout} ->
            ok
    end,
    self() ! accept,
    S.

start_handler(Socket) ->
    Pid = erlang:spawn_link(?MODULE, loop, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    {ok, Pid}.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            got_data(Data),
            loop(Socket);
        {tcp_closed, Socket} ->
            ?INFO_MSG("tcp_closed: ~p~n", [Socket]),
            ok
    after timer:seconds(5) ->
            ?INFO_MSG("after~n", []),
            loop(Socket)
    end.

got_data(Data) ->
    Ev = {_, DataCenter, JID} = deserialize_event(Data),
    ?INFO_MSG("event: ~p~n", [Ev]),
    R = #remote_session{bare_jid = {JID#jid.luser, JID#jid.lserver},
                        datacenter = DataCenter},
    mnesia:activity(sync_dirty, fun mnesia:write/1, [R]),
    ok.

deserialize_event(BEvent) ->
    case binary_to_term(BEvent) of
        {session_opened, _, _} = Ev -> Ev;
        {session_closed, _, _} = Ev -> Ev
    end.
