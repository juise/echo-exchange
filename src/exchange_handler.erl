-module(exchange_handler).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-include("yaws.hrl").
-include("yaws_api.hrl").

-export([out/1, handle_request/3]).


out(Arg) ->
	io:format("~p~n", [self()]),
	Request = Arg#arg.req,
	Method = Request#http_request.method,
	{_, Path} = Request#http_request.path,
	handle_request(Method, Path, Arg).


handle_request('GET', "/api/v1/get" ++ _, Arg) ->
	io:format("~p~n", [yaws_api:parse_query(Arg)]),
	Uri = yaws_api:request_url(Arg),
	Uri_path = Uri#url.path,Path = string:tokens(Uri_path, "/"),
	io:format("~p~n", [Path]),
	make_response(200, "<p>Index page </p>");

handle_request(_, Request, _Arg) -> % catchall
	{page, Request}.

make_response(Status, Message) ->
	make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
	make_all_response(Status, make_header(Type), Message).

make_header(Type) ->
	[{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
	[{status, Status}, {allheaders, Headers}, {html, Message}].

