%% CDDL HEADER START    -*-Erlang-*-
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Concatenate inputs and print to standard output.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.2

-define(module, cat).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.0.2").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(gen_command).
-import(string).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([loop/1, do_run/2]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?MODULE:loop(IO).

%%
%% Local Functions
%%

%%@private Export to allow for hotswap.
loop(IO) ->
  Stdin = IO#std.in,
  Stdin ! {stdin, self(), captln},
  receive
	{purging, _Pid, _Mod}					-> % chase your tail
	  ?MODULE:loop(IO);
    {'EXIT', Stdin, Reason}					->
	  ?DEBUG("cat: term: ~p~n", [Reason]), exit(ok);
	{'EXIT', _Pid, _Reason}					->
	  ?MODULE:loop(IO);
	{stdout, Stdin, ".\n"} when IO#std.stop	->
	  ?DEBUG("cat: eof\n"), exit(ok);
	{stdout, Stdin, eof}					-> 
	  ?DEBUG("cat: eof\n"), exit(ok);
	{stdout, Stdin, Line}					->
	  ?STDOUT(Line),
	  ?MODULE:loop(IO);
	Noise									->
	  ?STDERR("cat: noise: ~p~n", [Noise]),
	  ?MODULE:loop(IO)
  end.