%%%-------------------------------------------------------------------
%% @doc pollution top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    Monitor = pollution:createMonitor(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, Monitor).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(StartMonitor) ->
    Server = {pollution, {pollution_gen_server, start_link, [StartMonitor]},
    permanent, 2000, worker, [pollution_gen_server]},
    SupFlags = #{strategy => one_for_one,  % changed from one_for_all
    intensity => 3,
    period => 3},
    ChildSpecs = [Server],
    {ok, {SupFlags, ChildSpecs}}.

%%    SupFlags = #{strategy => one_for_all,
%%                 intensity => 0,
%%                 period => 1},
%%    ChildSpecs = [],
%%    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
