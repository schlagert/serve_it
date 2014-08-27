%%%=============================================================================
%%% Copyright 2014, Tobias Schlager <schlagert@github.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% @end
%%%=============================================================================
-module(serve_it).

-behaviour(application).
-behaviour(cowboy_http_handler).

%% Application callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    {ok, Cwd} = file:get_cwd(),
    BaseDir = application:get_env(?MODULE, base_dir, Cwd),
    Routes = cowboy_router:compile([{'_', [{"/[...]", ?MODULE, BaseDir}]}]),
    Options = [{compress, true}, {env, [{dispatch, Routes}]}],
    cowboy:start_http(?MODULE, 10, [{port, get_port()}], Options).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> cowboy:stop_listener(?MODULE).

%%%=============================================================================
%%% cowboy_http_handler callbacks
%%%=============================================================================

-record(state, {base_dir :: string()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({tcp, _}, Req, BaseDir) -> {ok, Req, #state{base_dir = BaseDir}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle(Req, State) ->
    {DecodedPath, LocalPath} = get_paths(Req, State),
    {ok,
     case {filelib:is_file(LocalPath), filelib:is_dir(LocalPath)} of
         {true, true} ->
             reply_html(200, dir_html(DecodedPath, LocalPath, State), Req);
         {true, false} ->
             reply_file(200, LocalPath, Req);
         {false, _} ->
             reply_html(404, not_found_html(DecodedPath), Req)
     end,
     State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _Req, #state{}) -> ok.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_paths(Req, #state{base_dir = BaseDir}) ->
    {RawPath, Req} = cowboy_req:path(Req),
    DecodedPath = [$/ | SubPath] = http_uri:decode(binary_to_list(RawPath)),
    {DecodedPath, filename:join([BaseDir, SubPath])}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_uri(Uri) ->
    Tokens = string:tokens(Uri, "/"),
    [$/ | string:join([http_uri:encode(T) || T <- Tokens], "/")].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
not_found_html(DecodedPath) ->
    [
     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">",
     "<html>",
     "<head><title>404 Not Found</title></head>",
     "<body>",
     "<h1>Not Found</h1>",
     "<p>The requested URL ", DecodedPath, " was not found.</p>",
     "<hr>",
     "</body>",
     server_html(),
     "</html>"
    ].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply_html(Status, Html, Req) ->
    Headers = [{<<"content-type">>, <<"text/html">>}],
    Resp = iolist_to_binary(Html),
    {ok, Req2} = cowboy_req:reply(Status, Headers, Resp, Req),
    Req2.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply_file(Status, FilePath, Req) ->
    FileSize = filelib:file_size(FilePath),
    Fun = fun(Socket, Transport) -> Transport:sendfile(Socket, FilePath) end,
    Req2 = cowboy_req:set_resp_body_fun(FileSize, Fun, Req),
    {Type, SubType, []} = cow_mimetypes:all(list_to_binary(FilePath)),
    Headers = [{<<"content-type">>, <<Type/binary, $/, SubType/binary>>}],
    {ok, Req3} = cowboy_req:reply(Status, Headers, Req2),
    Req3.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dir_html(DecodedPath, LocalPath, State) ->
    [
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">",
     "<html>",
     "<head><title>Index of ", DecodedPath, "</title></head>",
     "<body>"
     "<h1>Index of ", DecodedPath, "</h1>",
     "<table>",
     "<tr><th>Name</th>", "<th>Last modified</th>", "<th>Size</th></tr>",
     "<tr><th colspan=\"3\"><hr></th></tr>",
     parent_dir_html(DecodedPath, LocalPath, State),
     dir_entries_html(LocalPath),
     "<tr><th colspan=\"3\"><hr></th></tr>",
     "</table>",
     server_html(),
     "</body>",
     "</html>"
    ].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
parent_dir_html(_DecodedPath, BaseDir, #state{base_dir = BaseDir}) ->
    "";
parent_dir_html(DecodedPath, LocalPath, _State) ->
    ParentDir = filename:dirname(LocalPath),
    LastModified = filelib:last_modified(ParentDir),
    ParentPath = filename:dirname(filename:absname(DecodedPath)),
    [
     "<tr>",
     "<td><a href=\"",
     encode_uri(ParentPath),
     "\">Parent Directory</a></td>",
     "<td align=\"right\">",
     datetime:datetime_encode(LastModified, 'GMT', rss),
     "</td>",
     "<td align=\"right\">-</td>",
     "</tr>"
    ].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dir_entries_html(LocalPath) ->
    [dir_entry_html(LocalPath, F) || F <- filelib:wildcard("*", LocalPath)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dir_entry_html(LocalPath, FileName) ->
    FilePath = filename:join([LocalPath, FileName]),
    LastModified = filelib:last_modified(FilePath),
    case filelib:is_dir(FilePath) of
        true ->
            FileSize = "-",
            FileLabel = FileName ++ "/";
        false ->
            FileSize = [integer_to_list(filelib:file_size(FilePath)), "B"],
            FileLabel = FileName
    end,
    [
     "<tr>",
     "<td><a href=\"", http_uri:encode(FileName), "\">", FileLabel, "</a></td>",
     "<td align=\"right\">",
     datetime:datetime_encode(LastModified, 'GMT', rss),
     "</td>",
     "<td align=\"right\">", FileSize, "</td>",
     "</tr>"
    ].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
server_html() ->
    {ok, Hostname} = inet:gethostname(),
    [Version] = [V|| {cowboy, _, V} <- application:which_applications()],
    [
     "<address>Cowboy/", Version,
     " Server at ", Hostname, " Port ", integer_to_list(get_port()),
     "</address>"
    ].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_port() -> application:get_env(?MODULE, port, 8080).
