-module(http_server_util).
-export([reason_phrase/1, message/2, rfc1123_date/0, rfc1123_date/1, day/1, month/1, header/3, header/4,
	get_body/2, assembly_http_response/2]).

-define(SERVER_SOFTWARE, "http_server").
-define(CRLF, "\r\n").

-spec header(integer(), string(), boolean()) -> string().
header(StatusCode, MimeType, PersistentConnection) when is_integer(StatusCode) ->
	header(StatusCode, MimeType, PersistentConnection, rfc1123_date()).

header(StatusCode, MimeType, PersistentConnection, Date) when is_integer(StatusCode) ->
	Connection =
	case PersistentConnection of
		true ->
			"";
		_ ->
			"Connection: close \r\n"
	end,
	io_lib:format("HTTP/1.1 ~w ~s \r\nDate: ~s\r\nServer: ~s\r\n"
				  "Content-Type: ~s\r\n~s",
				  [StatusCode, reason_phrase(StatusCode),
				   Date, ?SERVER_SOFTWARE, MimeType, Connection]).

-spec reason_phrase(integer()) -> string().
reason_phrase(100) -> "Continue";
reason_phrase(101) -> "Swithing protocol";
reason_phrase(200) -> "OK";
reason_phrase(201) -> "Created";
reason_phrase(202) -> "Accepted";
reason_phrase(204) -> "No Content";
reason_phrase(205) -> "Reset Content";
reason_phrase(206) -> "Partial Content";
reason_phrase(301) -> "Moved Permanently";
reason_phrase(302) -> "Moved Temporarily";
reason_phrase(304) -> "Not Modified";
reason_phrase(400) -> "Bad Request";
reason_phrase(401) -> "Unauthorized";
reason_phrase(402) -> "Payment Required";
reason_phrase(403) -> "Forbidden";
reason_phrase(404) -> "Not Found";
reason_phrase(405) -> "Method Not Allowed";
reason_phrase(408) -> "Request Timeout";
reason_phrase(411) -> "Length Required";
reason_phrase(414) -> "Request-URI Too Long";
reason_phrase(412) -> "Precondition Failed";
reason_phrase(416) -> "request Range Not Satisfiable";
reason_phrase(417) -> "Expectation failed";
reason_phrase(500) -> "Internal Server Error";
reason_phrase(501) -> "Not Implemented";
reason_phrase(502) -> "Bad Gateway";
reason_phrase(503) -> "Service Unavailable";
reason_phrase(_)   -> "Internal Server Error".

-spec message(integer(), string()) -> string().
message(200, String) ->
	String;
message(500, String) ->
	"The server encountered an internal error or
  misconfiguration and was unable to complete
  your request. The reason is " ++ String;
message(503, String) ->
	"This service in unavailable due to: " ++ String.

-spec get_body(string(), string()) -> string().
get_body(ReasonPhrase, Message) ->
	"<HTML>
	   <HEAD>
		   <TITLE>" ++ ReasonPhrase ++ "</TITLE>
      </HEAD>
      <BODY>
      <H1>" ++ ReasonPhrase ++ "</H1>\n" ++ Message ++ "\n</BODY>
      </HTML>\n".

-spec assembly_http_response(integer(), string()) -> [string()].
assembly_http_response(Status, PhraseArgs) ->
	Header = header(Status, "text/html", false),
	ReasonPhrase = reason_phrase(Status),
	Message = message(Status, PhraseArgs),
	Body = get_body(ReasonPhrase, Message),
	FullHeader = Header ++
				 "Content-Length:" ++
				 integer_to_list(length(Body)) ++ ?CRLF ++ ?CRLF,
	[FullHeader, Body].

%% rfc1123_date

rfc1123_date() ->
	{{YYYY, MM, DD}, {Hour, Min, Sec}} = calendar:universal_time(),
	DayNumber = calendar:day_of_the_week({YYYY, MM, DD}),
	lists:flatten(io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
								[day(DayNumber), DD, month(MM), YYYY, Hour, Min, Sec])).

rfc1123_date({{YYYY, MM, DD}, {Hour, Min, Sec}}) ->
	DayNumber = calendar:day_of_the_week({YYYY, MM, DD}),
	lists:flatten(io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
								[day(DayNumber), DD, month(MM), YYYY, Hour, Min, Sec])).

%% day

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

%% month

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".