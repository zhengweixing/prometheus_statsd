-module(prometheus_statsd_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [{prometheus_statsd, {prometheus_statsd, start_link, []}, permanent, 5000, worker, [prometheus_statsd]}],
    {ok, {{one_for_one, 10, 100}, Children}}.
