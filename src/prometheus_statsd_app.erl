-module(prometheus_statsd_app).
-include("prometheus_statsd.hrl").

-behaviour(application).

-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, Sup} = prometheus_statsd_sup:start_link(),
    do_start(Sup).

stop(_State) ->
    ok.


do_start(Sup) ->
    prometheus_metrics:register_collectors(?DEFREGISTRY, [
        prometheus_mnesia_collector,
        prometheus_vm_memory_collector,
        prometheus_vm_statistics_collector,
        prometheus_vm_system_info_collector
    ]),



    {ok, Bin} = file:read_file(lists:concat([code:priv_dir(?MODULE), "/", Registry, ".json"])),
    Metrics = jsx:decode(Bin, [{labels, binary}, return_maps]),
    ok = prometheus_metrics:new(?DEFREGISTRY, Metrics),


    case application:get_env(?APP, push_gateway) of
        {ok, "http://"++ PushGateway0} ->
            PushGateway = "http://"++ PushGateway0,
            Mod = prometheus_statsd_client,
            Child = {Mod, {Mod, start_link, [PushGateway]}, permanent, 5000, worker, [Mod]},
            case supervisor:start_child(prometheus_statsd_sup, Child) of
                {ok, _} ->
                    {ok, Sup};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Port} ->
            Dispatch = cowboy_router:compile([
                {'_', [
                    {"/metrics/job/:Registry/instance/:Instance", prometheus_statsd_server, []},
                    {"/[...]", prometheus_statsd_server, all}
                ]}
            ]),
            {ok, _} = cowboy:start_clear(http, [{port, list_to_integer(Port)}], #{
                env => #{dispatch => Dispatch}
            }),
            {ok, Sup}
    end.
