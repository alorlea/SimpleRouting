%Modified by: Alberto

-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).


%%
%% map:new().
%%
%% map:update(berlin, [london, madrid], []).
%% map:update(berlin, [london, madrid], [{madrid,{london}}]).
%% map:update(berlin, [london, madrid], [{madrid,{london}}, {berlin, [london]}]).


%% The map is represented by a list of entries, each entry on the form 
%% {Node, Links}, wher Links is a list of nodes. Note that links are 
%% directlional so if london has a link to berlin does not mean that 
%% berlin has a link to london.

%% map() will generate an empty map

new() ->
     [].

%% update(Node, Links, Map) will update the Map given a new
%% set of links reachable from a node.
    
update(Node, Links, Map) ->
    case lists:keysearch(Node, 1, Map) of
	{value, {_, _}} ->
	    [{Node, Links}|lists:keydelete(Node, 1, Map)];
	false ->
	    [{Node, Links}|Map]
    end.


%% Lists the nodes reachable from a node.

reachable(Node, Map) ->
    case lists:keysearch(Node, 1, Map) of
	{value, {Node, Nodes}} ->
	    Nodes;
	false ->
	    []
    end.

%% Lists all nodes in the graph.

all_nodes(Map) ->
    lists:foldl(fun(E,Acc) -> add_all(E,Acc) end, [], Map).

add_all({Node, Links}, All) ->
    lists:foldl(fun(N,Acc) -> add_node(N,Acc) end, All, [Node|Links]).

add_node(Node, All) ->
    case lists:member(Node, All) of 
	true -> All; 
	false -> [Node|All] 
    end.