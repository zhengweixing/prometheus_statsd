-module(prometheus_statsd).
-include("prometheus_statsd.hrl").
-behaviour(gen_server).

%% Interface
-export([start_link/0, call/1]).

%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(TIMER_MSG, '#interval').

-record(state, {instances}).


call(Request) ->
    gen_server:call(?MODULE, Request, infinity).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{instances = []}}.


handle_call({add, Mod, Instance, Registry, Metrics}, _From, State = #state{instances = Instances}) ->
    NewMetrics =
        lists:foldl(
            fun(Info, Acc) ->
                case add(Mod, Instance, Registry, Info) of
                    {error, mf_already_exists} ->
                        Acc;
                    {error, Reason} ->
                        lager:warning("add metrics error, ~p,~p", [Info, Reason]),
                        Acc;
                    Spec ->
                        %lager:info("add metrics success, ~p", [Spec]),
                        [Spec | Acc]
                end
            end, [], Metrics),
    {reply, ok, State#state{instances = [{Registry, Instance, Mod, NewMetrics} | Instances]}};


handle_call(get_instances, _From, #state{instances = Instances} = State) ->
    {reply, {ok, Instances}, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.



add(Mod, Instance, Registry, Stat) ->
    case catch new_registry(Mod, Registry, Stat) of
        {'EXIT', {{mf_already_exists, _, _}, _}} ->
            {error, mf_already_exists};
        {'EXIT', {{mf_already_exists, _Reason}, _}} ->
            {error, mf_already_exists};
        {'EXIT', Reason} ->
            {error, Reason};
        Spec -> Spec
    end.


new_registry(Mod, Registry, #{<<"type">> := Type, <<"name">> := Name, <<"help">> := Help} = Stat) ->
    StrHelp = unicode:characters_to_list(Help),
    Labels = maps:get(<<"labels">>, Stat, []),
    Spec = #{
        name => Name,
        help => StrHelp,
        registry => Registry,
        labels => [Label || #{ <<"label">> := Label } <- Labels]
    },
    case Type of
        <<"histogram">> ->
            #{<<"bounds">> := Bounds} = Stat,
            collect_metrics(Mod, <<"histogram">>, Labels, Spec#{bounds => Bounds});
        _ ->
            collect_metrics(Mod, Type, Labels, Spec)
    end.


collect_metrics(Mod, <<"counter">>, Labels, Spec) ->
    prometheus_counter:new(maps:to_list(Spec)),
    do_callback(Mod, Spec#{ labels => Labels });
collect_metrics(Mod, <<"gauge">>, Labels, Spec) ->
    prometheus_gauge:new(maps:to_list(Spec)),
    do_callback(Mod, Spec#{ labels => Labels });
collect_metrics(Mod, <<"summary">>, Labels, Spec) ->
    prometheus_summary:new(maps:to_list(Spec)),
    do_callback(Mod, Spec#{ labels => Labels });
collect_metrics(Mod, <<"histogram">>, Labels, Spec) ->
    prometheus_histogram:new(maps:to_list(Spec)),
    do_callback(Mod, Spec#{ labels => Labels }).

do_callback(Mod, Spec) ->
    case Mod =/= undefined andalso erlang:function_exported(Mod, init_metrics, 1) of
        true ->
            apply(Mod, init_metrics, [Spec]);
        false ->
            ok
    end,
    maps:without([help, registry], Spec).
