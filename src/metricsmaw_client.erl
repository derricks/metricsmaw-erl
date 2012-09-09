% A gen_server that knows how to interact with metricsmaw servers. Those

-module(metricsmaw_client).
-behaviour(gen_server).

% exported methods for sending data
-export([increment_counter/1,increment_counter/2,decrement_counter/1,decrement_counter/2,set_gauge/2,mark_minute/1,mark_minute/2,current_value/1,start/1,start/0,
         all_metric_names/0,get_metric_details/1]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


% client-friendly methods that call into gen_server methods

% options is a proplist
%   supported options:
%   host: the host of the metricsmaw server, defaults to localhost
%   port: the port of the metricsmaw server, defaults to 18000
start() -> start([]).

start(Options) ->
	gen_server:start_link({local,?MODULE},?MODULE,Options,[]).
	
% increments the specified counter by 1
increment_counter(Counter) -> increment_counter(Counter,1).

% increments the specified counter by the specified amount
increment_counter(Counter,Amount) ->
	gen_server:cast(?MODULE,{add,Counter,counter,Amount}).
	
% decrement specified counter by 1
decrement_counter(Counter) -> decrement_counter(Counter,1).

% if you send a positive number to the function (which is implied by the name "decrement by X")
decrement_counter(Counter,Amount) when Amount > 0 -> increment_counter(Counter,-1 * Amount);
% but also handle the case where you pass a negative number thinking that you neeed to do that
decrement_counter(Counter,Amount) -> increment_counter(Counter,Amount).

set_gauge(Gauge,Amount) -> gen_server:cast(?MODULE,{add,Gauge,gauge,Amount}).

mark_minute(Meter) -> mark_minute(Meter,1).

mark_minute(Meter,Amount) -> gen_server:cast(?MODULE,{add,Meter,meter_minute,Amount}).

current_value(MetricName) -> gen_server:call(?MODULE,{current_value,MetricName}).

all_metric_names() -> gen_server:call(?MODULE,{all_metric_names}).

get_metric_details(MetricName) -> gen_server:call(?MODULE,{get_metric_details,MetricName}).

% private methods
connect_socket(Host,Port) ->
	case gen_tcp:connect(Host,Port,[binary,{packet,raw}]) of
		{ok,Socket} -> {Host,Port,Socket};
	    {error,Reason} ->
		    io:format("Could not connect to ~p:~p - ~p~n",[Host,Port,Reason]),
		    socket_error_state(Host,Port)
	end.
	
socket_error_state(Host,Port) -> {Host,Port,null}.
log_socket_error({error,Reason}) -> io:format("Error sending data over socket ~p~n",[Reason]).

% refactors out common logic for dealing with socket chatter to server
% sends MessageToSend to socket and, if it fails, invokes handle_call with MessageToHandleCall, etc.
gen_socket_comm(MessageToSend,MessageToHandleCall,From,{Host,Port,Socket}=State) ->
	case gen_tcp:send(Socket,term_to_binary(MessageToSend)) of
		ok -> 
		  receive
		   	{tcp,Socket,Data} ->
		    {reply,binary_to_term(Data),State}
	      end;
		{error,_Reason}=TcpError ->
			gen_tcp:close(Socket),
			log_socket_error(TcpError),
			handle_call(MessageToHandleCall,From,socket_error_state(Host,Port))
		end.
	
% gen_server behaviour methods

% On init, connect to the server and return a tuple of Host, Port, Socket (in case Socket gets disconnected)
init(Options) ->
	Host = proplists:get_value(host,Options,'localhost'),
	Port = proplists:get_value(port,Options,18000),
	
	% if can't connect to socket, hold off until it's actually used
	{ok,connect_socket(Host,Port)}.
	
% only one call is currently supported: current_value	
% first match any case where socket is null, try to connect
handle_call(Request,From,{Host,Port,null}) ->
	SocketConnectResponse = connect_socket(Host,Port),
	case SocketConnectResponse of
		 % no connect = respond with undefined
		 {Host,Port,null} -> {reply,undefined,SocketConnectResponse};
		 {Host,Port,_Socket} -> handle_call(Request,From,SocketConnectResponse)
    end;

handle_call({current_value,MetricName}=Metric,From,State) ->
	gen_socket_comm({get,MetricName},Metric,From,State);
	
% for the generic case where the Request is both what to send over the socket and what to pass along to handle_call if the socket is closed
handle_call(Request,From,State) 
    when is_tuple(Request), element(1,Request) =:= all_metric_names;
         is_tuple(Request), element(1,Request) =:= get_metric_details ->
	gen_socket_comm(Request,Request,From,State).
		
% only one cast currently supported: add data	
% as with handle_call, check undefined socket first
handle_cast(Request,{Host,Port,null}) ->
	SocketConnectResponse = connect_socket(Host,Port),
	case SocketConnectResponse of 
		{Host,Port,null} -> {noreply,SocketConnectResponse};
		{Host,Port,_Socket} -> handle_cast(Request,SocketConnectResponse)
	end;
handle_cast({add,_MetricName,_MetricType,_Data}=Request,{Host,Port,Socket}=State) ->
	case gen_tcp:send(Socket,term_to_binary(Request)) of
		ok -> {noreply,State};
		{error,_Reason}=TcpError ->
			log_socket_error(TcpError),
			% see note in handle_call
			handle_cast(Request,socket_error_state(Host,Port))
	end.
	
handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,{_Host,_Port,null}) -> ok;
terminate(_Reason,{_Host,_Port,Socket}) -> gen_tcp:close(Socket),ok.

code_change(_Old,State,_Extra) -> {ok,State}.
	
