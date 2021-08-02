-module(prometheus_metrics).
-include("prometheus_statsd.hrl").
-export([counter/2, counter/3, counter/4, gauge/2, gauge/3, gauge/4, summary/2, summary/3, summary/4, histogram/3, histogram/2, histogram/4]).
-export([counter_reset/1, counter_reset/2, counter_reset/3]).
-export([gauge_reset/1, gauge_reset/2, gauge_reset/3]).
-export([summary_reset/1, summary_reset/2, summary_reset/3]).
-export([histogram_reset/1, histogram_reset/2, histogram_reset/3]).
-export([register_collectors/2, new/2, new/3, query_range/5, collect/2, collect2/2]).



new(Registry, Metrics) ->
    [_Name, Instance] = string:tokens(atom_to_list(node()), "@"),
    prometheus_statsd:call({add, undefined, Instance, Registry, Metrics}).


new(Registry, Mod, Metrics) ->
    [_Name, Instance] = string:tokens(atom_to_list(node()), "@"),
    prometheus_statsd:call({add, Mod, Instance, Registry, Metrics}).



register_collectors(Registry, NewCollectors) ->
    Collectors = [
        prometheus_counter,
        prometheus_gauge,
        prometheus_summary,
        prometheus_histogram
    ],
    prometheus_registry:register_collectors(Registry, Collectors ++ NewCollectors).


query_range(Version, Query, Start, End, Step) ->
    {ok, Host} = application:get_env(?APP, prometheus_server),
    Q = cow_uri:urlencode(Query),
    Path = binary_to_list(<<"/api/", Version/binary, "/query_range?query=", Q/binary>>),
    Url = lists:concat([Host, Path, "&start=", Start, "&end=", End, "&step=", Step]),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_HTTPVersion, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
                {'EXIT', Reason} ->
                    {error, Reason};
                Data ->
                    {ok, StatusCode, Data}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


collect2(Registry, Instance) ->
    {ok, Instances} = prometheus_statsd:call(get_instances),
    collect2(Instances, Registry, Instance).
collect2([{Registry, Instance, Mod, Metrics} | _], Registry, Instance) ->
    case erlang:function_exported(Mod, collect_metrics, 4) of
        true ->
            [Mod:collect_metrics(Instance, Registry, Name, Labels) || #{name := Name, labels := Labels} <- Metrics];
        false ->
            ok
    end,
    prometheus_text_format:format(Registry);
collect2([], _Registry, _Instance) ->
    <<>>;
collect2([_ | Instances], Registry, Instance) ->
    collect2(Instances, Registry, Instance).


collect(Fun, Acc0) ->
    {ok, Instances} = prometheus_statsd:call(get_instances),
    lists:foldl(
        fun({Registry, Instance, Mod, Metrics}, Acc1) ->
            Data = collect2([{Registry, Instance, Mod, Metrics}], Registry, Instance),
            Fun({Registry, Instance, Data}, Acc1)
        end, Acc0, Instances).


counter(Name, Value) ->
    counter(Name, [], Value).
counter(Name, LabelValues, Value) ->
    counter(?DEFREGISTRY, Name, LabelValues, Value).
counter(Registry, Name, LabelValues, Value) ->
    prometheus_counter:inc(Registry, Name, LabelValues, Value).



counter_reset(Name) ->
    counter_reset(Name, []).
counter_reset(Name, LabelValues) ->
    counter_reset(?DEFREGISTRY, Name, LabelValues).
counter_reset(Registry, Name, LabelValues) ->
    prometheus_counter:reset(Registry, Name, LabelValues).



gauge(Name, Value) ->
    gauge(Name, [], Value).
gauge(Name, LabelValues, Value) ->
    gauge(?DEFREGISTRY, Name, LabelValues, Value).
gauge(Registry, Name, LabelValues, Value) ->
    prometheus_gauge:set(Registry, Name, LabelValues, Value).


gauge_reset(Name) ->
    gauge_reset(Name, []).
gauge_reset(Name, LabelValues) ->
    gauge_reset(?DEFREGISTRY, Name, LabelValues).
gauge_reset(Registry, Name, LabelValues) ->
    prometheus_gauge:reset(Registry, Name, LabelValues).


summary(Name, Value) ->
    summary(Name, [], Value).
summary(Name, LabelValues, Value) ->
    summary(?DEFREGISTRY, Name, LabelValues, Value).
summary(Registry, Name, LabelValues, Value) ->
    prometheus_summary:observe(Registry, Name, LabelValues, Value).


summary_reset(Name) ->
    summary_reset(Name, []).
summary_reset(Name, LabelValues) ->
    summary_reset(?DEFREGISTRY, Name, LabelValues).
summary_reset(Registry, Name, LabelValues) ->
    prometheus_summary:reset(Registry, Name, LabelValues).


histogram(Name, Value) ->
    histogram(Name, [], Value).
histogram(Name, LabelValues, Value) ->
    histogram(?DEFREGISTRY, Name, LabelValues, Value).
histogram(Registry, Name, LabelValues, Value) ->
    prometheus_histogram:observe(Registry, Name, LabelValues, Value).


histogram_reset(Name) ->
    histogram_reset(Name, []).
histogram_reset(Name, LabelValues) ->
    histogram_reset(?DEFREGISTRY, Name, LabelValues).
histogram_reset(Registry, Name, LabelValues) ->
    prometheus_histogram:reset(Registry, Name, LabelValues).
