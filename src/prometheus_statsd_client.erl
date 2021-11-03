-module(prometheus_statsd_client).

-include("prometheus_statsd.hrl").

-behaviour(gen_server).

%% Interface
-export([start_link/1]).

%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(TIMER_MSG, '#interval').

-record(state, {push_gateway, timer, interval}).


start_link(PushGateway) ->
    Interval = application:get_env(?APP, interval, 5000),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PushGateway, Interval], []).

init([PushGateway, Interval]) ->
    Ref = erlang:start_timer(Interval, self(), ?TIMER_MSG),
    {ok, #state{timer = Ref,  push_gateway = PushGateway, interval = Interval}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, R, ?TIMER_MSG}, S = #state{interval = I, timer = R, push_gateway = Uri}) ->
    Fun =
        fun({Registry, Instance, Data}, Acc) ->
            push_to_gateway(Uri, Registry, Instance, Data),
            [{Registry, Instance}|Acc]
        end,
    catch prometheus_metrics:collect(Fun, []),
    {noreply, S#state{timer = erlang:start_timer(I, self(), ?TIMER_MSG)}};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

push_to_gateway(Uri, Registry, Instance, Data) ->
    Url = lists:concat([Uri, "/metrics/job/", Registry, "/instance/", Instance]),
    case http_request(Url, Data, 30000) of
        ok ->
            ok;
        {error, Reason} ->
            lager:error("push to ~s error, reason:~p~n", [Uri, Reason])
    end.

http_request(Url, Data, Timeout) ->
    case httpc:request(post, {Url, [], "text/plain", Data}, [{autoredirect, true}, {timeout, Timeout}], []) of
        {ok, {{_, HTTPCode, _}, _, _Body}} when HTTPCode == 202; HTTPCode == 200 ->
            ok;
        {error, {failed_connect, _}} ->
            {error, econnrefused};
        {error, Reason} ->
            {error, Reason}
    end.
