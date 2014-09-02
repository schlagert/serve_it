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
%%% A very simple web server based on `cowboy' to serve a configured directory
%%% (by default the `CWD'), providing Apache-like basics like directory listing,
%%% `range' header (aka file streaming) and index file (aka `index.html')
%%% support.
%%%
%%% Configuration (application environment):
%%% * port (integer): server listen port (default 8080)
%%% * base_dir (string): webserver root directory (default c:pwd())
%%% * index_file (string): basename of index file (default "index.*")
%%% * show_hidden_files (boolean): show hidden files/directories, hidden means
%%%   the name starts with a dot (default false)
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

-define(FOLDER_ICO, "006 Folder.png").
-define(FILE_ICO,   "020 Document.png").
-define(TEXT_ICO,   "021 Document Text.png").
-define(MUSIC_ICO,  "083 Music.png").
-define(PHOTO_ICO,  "084 Photo.png").
-define(MOVIE_ICO,  "085 Movie.png").
-define(BACK_ICO,   "127 ArrowLeft.png").

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    {ok, Cwd} = file:get_cwd(),
    Port = application:get_env(?MODULE, port, 8080),
    Args = [Port, application:get_env(?MODULE, base_dir, Cwd)],
    Routes = cowboy_router:compile([{'_', [{"/[...]", ?MODULE, Args}]}]),
    Options = [{compress, true}, {env, [{dispatch, Routes}]}],
    cowboy:start_http(?MODULE, 10, [{port, Port}], Options).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> cowboy:stop_listener(?MODULE).

%%%=============================================================================
%%% cowboy_http_handler callbacks
%%%=============================================================================

-record(state, {
          base_dir :: file:filename(),
          hostname :: inet:hostname(),
          port     :: inet:port_number()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({tcp, _}, Req, [Port, BaseDir]) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, Req, #state{base_dir = BaseDir, hostname = Hostname, port = Port}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle(Req, State) ->
    {DecodedPath, LocalPath} = get_paths(Req, State),
    {ok, NewReq} =
        case {filelib:is_file(LocalPath), filelib:is_dir(LocalPath)} of
            {true, true} ->
                case has_index_file(LocalPath) of
                    {true, IndexFile} ->
                        reply_file(IndexFile, Req);
                    false ->
                        reply_html(200, dir_html(DecodedPath, LocalPath, State), Req)
                end;
            {true, false} ->
                reply_file(LocalPath, Req);
            {false, _} ->
                reply_html(404, not_found_html(DecodedPath, State), Req)
        end,
    {ok, NewReq, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _Req, #state{}) -> ok.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Returns a tuple containing the decoded request path as well as its
%% corresponding path on disk.
%%------------------------------------------------------------------------------
get_paths(Req, State) ->
    {RawPath, Req} = cowboy_req:path(Req),
    get_paths_(http_uri:decode(binary_to_list(RawPath)), State).
get_paths_(DecodedPath = [$/ | ?FOLDER_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?FOLDER_ICO])};
get_paths_(DecodedPath = [$/ | ?FILE_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?FILE_ICO])};
get_paths_(DecodedPath = [$/ | ?TEXT_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?TEXT_ICO])};
get_paths_(DecodedPath = [$/ | ?MUSIC_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?MUSIC_ICO])};
get_paths_(DecodedPath = [$/ | ?PHOTO_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?PHOTO_ICO])};
get_paths_(DecodedPath = [$/ | ?MOVIE_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?MOVIE_ICO])};
get_paths_(DecodedPath = [$/ | ?BACK_ICO], _State) ->
    {DecodedPath, filename:join([code:priv_dir(?MODULE), ?BACK_ICO])};
get_paths_(DecodedPath = [$/ | SubPath], #state{base_dir = BaseDir}) ->
    {DecodedPath, filename:join([BaseDir, SubPath])}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
has_index_file(LocalPath) ->
    Index = application:get_env(?MODULE, index_file, "index.*"),
    case filelib:wildcard(filename:join(LocalPath, Index)) of
        [IndexFile | _] ->
            {true, IndexFile};
        [] ->
            false
    end.

%%------------------------------------------------------------------------------
%% @private
%% The markup returned on 404.
%%------------------------------------------------------------------------------
not_found_html(DecodedPath, State) ->
    [
     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n",
     "<html>\n",
     "<head><title>404 Not Found</title></head>\n",
     "<body>\n",
     "<h1>Not Found</h1>\n",
     "<p>The requested URL ", DecodedPath, " was not found.</p>\n",
     "<hr>\n",
     "</body>\n",
     server_html(State),
     "</html>\n"
    ].

%%------------------------------------------------------------------------------
%% @private
%% Sends a response containing HTML markup.
%%------------------------------------------------------------------------------
reply_html(Status, Html, Req) ->
    Headers = [{<<"content-type">>, <<"text/html">>}],
    Resp = iolist_to_binary(Html),
    cowboy_req:reply(Status, Headers, Resp, Req).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply_file(FilePath, Req0) ->
    FileSize = filelib:file_size(FilePath),
    case cowboy_req:header(<<"range">>, Req0) of
        {Range, Req0} when Range =:= undefined; Range =:= <<>> ->
            Req1 = set_resp_body_fun(FileSize, FilePath, Req0),
            Headers = file_headers(FileSize, FilePath),
            cowboy_req:reply(200, Headers, Req1);
        {<<"bytes=", Range/binary>>, Req0} ->
            case split_range(Range) of
                [F] when F >= 0, F < FileSize ->
                    L = FileSize - 1;
                [F, L] when F >= 0, L > 0, L < FileSize, F < L ->
                    ok
            end,
            Req1 = set_resp_body_fun(F, L - F + 1, FilePath, Req0),
            Headers = file_headers(F, L, FileSize, FilePath),
            cowboy_req:reply(206, Headers, Req1)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
split_range(Range) ->
    [list_to_integer(I) || I <- string:tokens(binary_to_list(Range), "-")].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_resp_body_fun(Size, FilePath, Req) ->
    Fun = fun(S, T) -> T:sendfile(S, FilePath) end,
    cowboy_req:set_resp_body_fun(Size, Fun, Req).
set_resp_body_fun(First, Size, FilePath, Req) ->
    Fun = fun(S, T) -> T:sendfile(S, FilePath, First, Size, []) end,
    cowboy_req:set_resp_body_fun(Size, Fun, Req).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
file_headers(Size, Path) ->
    file_headers(0, Size - 1, Size, Path).
file_headers(First, Last, Size, Path) ->
    {Type, SubType, []} = cow_mimetypes:all(list_to_binary(Path)),
    [{<<"content-type">>, <<Type/binary, $/, SubType/binary>>},
     {<<"content-length">>, integer_to_binary(Last - First + 1)},
     {<<"accept-ranges">>, <<"bytes">>},
     {<<"content-range">>, <<"bytes ",
                             (integer_to_binary(First))/binary, $-,
                             (integer_to_binary(Last))/binary, $/,
                             (integer_to_binary(Size))/binary>>}].

%%------------------------------------------------------------------------------
%% @private
%% The markup returned when requesting a directory path.
%%------------------------------------------------------------------------------
dir_html(DecodedPath, LocalPath, State) ->
    [
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n",
     "<html>\n",
     "<head><title>Index of ", DecodedPath, "</title></head>\n",
     "<body>\n"
     "<h1>Index of ", DecodedPath, "</h1>\n",
     "<table>\n",
     "<tr><th></th><th>Name</th><th>Last modified</th><th>Size</th></tr>\n",
     "<tr><th colspan=\"4\"><hr></th></tr>\n",
     parent_dir_html(DecodedPath),
     dir_entries_html(LocalPath),
     "<tr><th colspan=\"4\"><hr></th></tr>\n",
     "</table>\n",
     server_html(State),
     "</body>\n",
     "</html>\n"
    ].

%%------------------------------------------------------------------------------
%% @private
%% The markup for the parent directory link.
%%------------------------------------------------------------------------------
parent_dir_html("/") ->
    "";
parent_dir_html(DirPath) ->
    [_ | RestPath] = lists:reverse(string:tokens(DirPath, "/")),
    EncRestPath = [http_uri:encode(P) || P <- lists:reverse(RestPath)],
    ParentPath = "/" ++ [P ++ "/" || P <- EncRestPath],
    [
     "<tr>\n",
     "<td>", image_html(?BACK_ICO), "</td>\n",
     "<td><a href=\"", ParentPath, "\">Parent Directory</a></td>\n",
     "<td colspan=\"2\"></td>\n"
     "</tr>\n"
    ].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dir_entries_html(LocalPath) ->
    Files = filelib:wildcard("*", LocalPath),
    ShowHidden = application:get_env(?MODULE, show_hidden_files, false),
    [dir_entry_html(ShowHidden, LocalPath, F) || F <- Files].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dir_entry_html(false, _LocalPath, [$. | _]) ->
    "";
dir_entry_html(_ShowHidden, LocalPath, FileName) ->
    FilePath = filename:join([LocalPath, FileName]),
    LastModifiedDate = filelib:last_modified(FilePath),
    LastModified = datetime:datetime_encode(LastModifiedDate, 'GMT', rss),
    case filelib:is_dir(FilePath) of
        true ->
            dir_dir_html(FileName, LastModified);
        false ->
            dir_file_html(FileName, FilePath, LastModified)
    end.

%%------------------------------------------------------------------------------
%% @private
%% The markup for a directory entry in the directory listing.
%%------------------------------------------------------------------------------
dir_dir_html(FileName, LastModified) ->
    [
     "<tr>\n",
     "<td>", image_html(?FOLDER_ICO), "</td>\n",
     "<td><a href=\"", http_uri:encode(FileName), "/\">", FileName, "/</a></td>\n",
     "<td align=\"right\">", LastModified, "</td>\n",
     "<td align=\"right\">-</td>\n",
     "</tr>\n"
    ].

%%------------------------------------------------------------------------------
%% @private
%% The markup for a file entry in the directory listing.
%%------------------------------------------------------------------------------
dir_file_html(FileName, FilePath, LastModified) ->
    FileSize = [integer_to_list(filelib:file_size(FilePath)), "B"],
    [
     "<tr>\n",
     "<td>", image_html(file_icon(FilePath)), "</td>\n",
     "<td><a href=\"", http_uri:encode(FileName), "\">", FileName, "</a></td>\n",
     "<td align=\"right\">", LastModified, "</td>\n",
     "<td align=\"right\">", FileSize, "</td>\n",
     "</tr>\n"
    ].

%%------------------------------------------------------------------------------
%% @private
%% The markup for the server footer.
%%------------------------------------------------------------------------------
server_html(#state{hostname = Hostname, port = Port}) ->
    [
     "<address>Cowboy/",
     hd([V|| {cowboy, _, V} <- application:which_applications()]),
     " Server at ", Hostname, " Port ", integer_to_list(Port),
     "</address>\n"
    ].

%%------------------------------------------------------------------------------
%% @private
%% Returns an icon based on the detected MIME type of a file path.
%%------------------------------------------------------------------------------
file_icon(FilePath) ->
    case cow_mimetypes:all(list_to_binary(FilePath)) of
        {<<"text">>, _, _}  -> ?TEXT_ICO;
        {<<"video">>, _, _} -> ?MOVIE_ICO;
        {<<"image">>, _, _} -> ?PHOTO_ICO;
        {<<"audio">>, _, _} -> ?MUSIC_ICO;
        _                   -> ?FILE_ICO
    end.

%%------------------------------------------------------------------------------
%% @private
%% The markup for an image icon.
%%------------------------------------------------------------------------------
image_html(Icon) -> ["<img src=\"/", Icon, "\" width=\"22\">"].
