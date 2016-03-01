-module(sonar).
-export([start/0, beep/2, period/1]).


period(PID)->
	spawn(sonar,beep,[self(),done]),
	receive done -> PID ! done 
	end.


                               
beep(PID,Signal) ->
	io:format("Beep On~n",[]),
	receive
	after 2 -> io:format("Beep Off~n",[]),
		   PID ! Signal
	end.
          
start() -> 
	spawn(sonar, period, [self()]),
	receive done-> timer:sleep(3000),
		       start()
	end.
 

	
