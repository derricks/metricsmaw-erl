metricsmaw-erl
==============

An Erlang client for a metricsmaw server. While metricsmaw has client methods in it, they're primarily for testing. This provides a separate, lightweight program
for sending metrics to a server. The client is itself a gen_server implementation.

The module is metricsmaw_client. Use this name to prefix the methods below.

Methods
-------

* start().

    Starts the client with default options (host = localhost, port = 18000)
    
  
* start([{host,Host},{port,Port}]).

    Starts the client with the specified options. host defaults to localhost; port defaults to 18000
    
  
* increment_counter(Name).

    Increments the counter with the given name.
    
  
* increment_counter(Name,Amount).

    Increments the given counter by the specified amount
    
  
* decrement_counter(Name).

    Decrements the named counter by 1
    
  
* decrement_counter(Name,Amount).

    Decrements the named counter by the specified amount
    
  
* set_gauge(Name,Amount).

    Sets the value of the gauge to the amount
    
  
* mark_minute(Name).

    Adds one to the specified per-minute meter
    
  
* mark_minute(Name,Amount).

    Adds the given amount to the specified per-minute meter.
  

