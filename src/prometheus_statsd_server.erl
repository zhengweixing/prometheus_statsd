%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 02. 8月 2021 8:40 下午
%%%-------------------------------------------------------------------
-module(prometheus_statsd_server).
-author("weixingzheng").

%% API
-export([init/2]).


init(Req0, all) ->
    Fun =
        fun({Registry0, Instance0, _Data}, Acc) ->
            Registry = list_to_binary(io_lib:format("~s", [Registry0])),
            Instance = list_to_binary(io_lib:format("~s", [Instance0])),
            [#{
                registry => Registry,
                instance => Instance,
                path => <<"/metrics/job/", Registry/binary, "/instance/", Instance/binary>>
            } | Acc]
        end,
    Header = #{
        <<"content-type">> => <<"text/plain; charset=utf-8">>
    },
    Req =
        case catch prometheus_metrics:collect2(Fun, []) of
            {'EXIT', Reason} ->
                Error = io_lib:format("~p", [Reason]),
                cowboy_req:reply(400, Header, list_to_binary(Error), Req0);
            Data ->
                cowboy_req:reply(200, Header, jsx:encode(Data), Req0)
        end,
    {ok, Req, all};

init(Req0, Opts) ->
    Registry = cowboy_req:binding('Registry', Req0),
    Instance = cowboy_req:binding('Instance', Req0),
    Header = #{
        <<"content-type">> => <<"text/plain; charset=utf-8">>
    },
    Req =
        case catch prometheus_metrics:collect2(binary_to_atom(Registry,utf8), binary_to_list(Instance)) of
            {'EXIT', Reason} ->
                Error = io_lib:format("~p", [Reason]),
                cowboy_req:reply(400, Header, list_to_binary(Error), Req0);
            Data ->
                cowboy_req:reply(200, Header, Data, Req0)
        end,
    {ok, Req, Opts}.
