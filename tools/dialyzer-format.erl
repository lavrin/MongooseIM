#!/usr/bin/env escript
%%!
%% -*- erlang -*-
%% vim: ft=erlang

-module('dialyzer-format').
-compile([export_all]).
-mode(compile).

-define(a2b(A), atom_to_binary(A, utf8)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

main(_Args) ->
    loop(standard_io).

loop(Handle) ->
    case read_term(Handle) of
        eof -> ok;
        {ok, Term} -> print("~s~n", [format(Term)]),
                      loop(Handle)
    end.

print(Fmt, Args) ->
    io:format(Fmt, Args).

print(IODev, Fmt, Args) ->
    io:format(IODev, Fmt, Args).

stderr(Fmt, Args) -> print(standard_error, Fmt, Args).

format({WarningType, Loc, Details}) ->
    case erlang:function_exported(?MODULE, WarningType, 2) of
        false ->
            {File, Line} = Loc,
            io_lib:format("~s:~b: ~p ~w", [File, Line, WarningType, Details]);
        true ->
            case ?MODULE:WarningType(Loc, Details) of
                {Fmt, Args} -> ?il2b(io_lib:format(Fmt, Args));
                IOList -> ?il2b(IOList)
            end
    end.

warn_callgraph(L, {call_to_missing, [M, F, A]}) ->
    [loc(L), "Call to missing or unexported function ", mfa(M,F,A)].

warn_behaviour(L, {callback_spec_type_mismatch, [M, F, A, T1, T2]}) ->
    case is_unreadable(T1) orelse is_unreadable(T2) of
        false -> 
            [loc(L), warn_behaviour_m(T1, T2, M, F, A, [])];
        true ->
            [loc(L), warn_behaviour_m("t1()", "t2()", M, F, A,
                                      [":\n",
                                       "    t1() :: ", T1, "\n",
                                       "    t2() :: ", T2])]
    end.

warn_behaviour_m(T1, T2, M, F, A, Extra) ->
    ["The return type ", T1, " in the specification of ",
     fa(F,A), " is not a subtype of ", T2, ", ",
     "which is the expected return type for the callback of ", ?a2b(M), " behaviour", Extra].

loc({File, Line}) -> [File, ":", ?i2b(Line), ": "].

mfa(M, F, A) -> [?a2b(M), ":", ?a2b(F), "/", ?i2b(A)].

fa(F, A) -> [?a2b(F), "/", ?i2b(A)].

is_unreadable(T) when is_list(T), length(T) > 10 -> true;
is_unreadable(T) when is_list(T) -> false.

read_term(Handle) ->
    read_term(Handle, start, file:read_line(Handle)).

read_term(_Handle, start, eof) -> eof;
read_term(_Handle, _, {error, Reason}) ->
    stderr("error: ~p~n", [Reason]);
read_term(Handle, Cont, {ok, Data}) ->
    case if_discard(Data) of
        true -> read_term(Handle, Cont, file:read_line(Handle));
        false -> do_read_term(Handle, Cont, Data)
    end.

do_read_term(Handle, Cont, Data) ->
    case io_lib:get_until(Cont, Data, unicode, {erl_scan, tokens, [1]}) of
        {stop, {ok, Tokens, _LeftOvers}, _Buf} ->
            erl_parse:parse_term(Tokens);
        NCont ->
            read_term(Handle, NCont, file:read_line(Handle))
    end.

if_discard("  Proceeding with analysis...\n") -> true;
if_discard(_) -> false.
