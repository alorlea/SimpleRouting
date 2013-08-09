%%Modified by: Alberto

-module(hist).

-export([new/1, update/3]).


new(Name) ->
    [{Name, 0}].


%% you can update the set of messages and the 
%% returned value is either a new set or the 
%% atom old.
    
update(Name, X, History)->
    case lists:keysearch(Name, 1, History) of
	{value, {Name, Y}} ->
	    if 
		X > Y -> 
		    {new, [{Name, X}|lists:keydelete(Name, 1, History)]};
		true -> 
		    old
	    end;
	false ->
	    {new, [{Name, 0}|lists:keydelete(Name, 1, History)]}
    end.