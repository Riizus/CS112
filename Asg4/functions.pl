/* Implementation of Not */
not( X ) :- X, !, fail.
not( _ ).

/* Degrees and Minutes to Radians */
degmin_in_rads( degmin( Degrees, Minutes ), Rads ) :-
    Degs is Degrees + Minutes / 60,
    Rads is Degs * pi / 180.

/* Puts Time in Decimal Hours */
hours_only(time( Hours, Mins ), Hoursonly ) :-
    Hoursonly is Hours + Mins / 60.

/* Converts distance to time with velocity of 500mph */
miles_to_hour( Miles, Hours ) :-
    Hours is Miles / 500.

/* Haversine Distance formula */
haversine( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3959. 

/* Finds distance between two airports */
distance( Start, End, Dist ) :-
   airport( Start, _, Lat1, Lon1 ),
   airport( End, _, Lat2, Lon2 ),
   degmin_in_rads( Lat1, Rad_Lat1 ),
   degmin_in_rads( Lat2, Rad_Lat2 ),
   degmin_in_rads( Lon1, Rad_Lon1 ),
   degmin_in_rads( Lon2, Rad_Lon2 ),
   haversine( Rad_Lat1, Rad_Lon1, Rad_Lat2, Rad_Lon2, Dist).

/* Prints numbers in correct format */
print_dig( Time ) :-
    Time < 10, print( 0 ), print( Time ).
print_dig( Time ) :-
    Time >= 10, print( Time ).

/* Prints the time */
print_time( Time ) :-
    Hourmin is floor( Time * 60 ),
    Hour is Hourmin // 60,
    Min is Hourmin mod 60,
    print_dig( Hour ), print( ':' ), print_dig( Min ).

/* Recursively finds paths to destination */
find_path([], EndPort, List, Time).

find_path(EndPort, EndPort, List, Time) :-
	find_path([], EndPort, [List | EndPort], Time).

find_path(CurrPort, EndPort, List, Time) :-
	flight( CurrPort, EndPort, StartTimeHM ),
    not( member( EndPort, List ) ),

    hours_only(StartTimeHM, StartTime),
    NewTime is Time + 0.5,
    StartTime > NewTime,

    distance( CurrPort, EndPort, Distance ),
    miles_to_hour( Distance, FlightTime ),

    EndTime is StartTime + FlightTime,
    EndTime < 24.0,
    findpath(End, End, [End | Visited], List, _).
    find_path(EndPort, EndPort, [List | [CurrPort, StartTime, EndTime]], EndTime).

find_path(CurrPort, EndPort, Path, [[CurrPort, StartTime, EndTime] | List], Time) :-
	flight( CurrPort, NextPort, StartTimeHM ),
    not( member( NextPort, Path ) ),

    hours_only(StartTimeHM, StartTime),
    NewTime is Time + .5,
    StartTime > NewTime,

    distance( CurrPort, NextPort, Distance ),
    miles_to_hour( Distance, FlightTime ),

    EndTime is StartTime + FlightTime,
    EndTime < 24.0,
    find_path(NextPort, EndPort, 
    	[NextPort | Path], List, EndTime).

/* Prints all stops along path */
print_path( [] ) :-
    nl.

print_path( [[X, X_Dep, X_Arr], Y | []] ) :-
    airport( X, XPort, _, _), airport( Y, YPort, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( X ), write( '  ' ),
    write( XPort ), print_time( X_Dep ), nl,
    write( '     ' ), write( 'arrive  ' ),
    write( Y ), write( '  ' ),
    write( YPort ), print_time( X_Arr ), nl,
    !, true.

print_path( [[X, X_Dep, X_Arr], [Y, Y_Dep, Y_Arr] | Z] ) :-
    airport( X, XPort, _, _), airport( Y, YPort, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( X ), write( '  ' ),
    write( XPort ), print_time( X_Dep ), nl,
    write( '     ' ), write( 'arrive  ' ),
    write( Y ), write( '  ' ),
    write( YPort ), print_time( X_Arr ), nl,
    !, print_path( [[Y, Y_Dep, Y_Arr] | Z] ).

/* Finds shortest path by endtime */
min_path([[Temp, Time]|Tail], Path) :-
    min_path(Tail, Time, Path).

min_path([], Min, Path).

min_path([[Temp, Time]|Tail], Min, Path) :-
    Min =< Time,
    min_path(Tail, Min, Path).

min_path([[Temp, Time]|Tail], Min, Path) :-
    Min > Time,
    min_path(Tail, Time, Temp).

/* Prints error if same start and end */
fly( Start, Start ) :-
    write( 'Error: the departure and arrival of: ' ), write(Start),
    write( ' to '), write(Start), write( ' are the same.' ),
    nl,
    !, fail.

/* Prints flight schedule with haversine formula */
fly( Start, End ) :-
    airport( Start, _, _, _ ),
    airport( End, _, _, _ ),

    setof([Path, Time], find_path(Start, End, Path, Time), List),
    !, nl,
    min_path(List, Route),
    print_path( Route ),
    true.

/* Prints error if flight path cannot be determined */
fly( Start, End ) :-
    airport( Start, _, _, _ ),
    airport( End, _, _, _ ),
    write( 'Error: flight from: ' ), write(Start),
    write( ' to '), write(End), write( ' is not possible.' ),
    !, fail.

/* Prints an error if airport cannot be found in database */
fly( _, _) :-
    write( 'Error: nonexistent airports.' ), nl,
!, fail.
