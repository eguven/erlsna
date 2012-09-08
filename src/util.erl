-module(util).
-export([timestring_to_timestamp/1, timestamp_to_timestring/1,
         timestamp_diff/2, add_seconds_to_ts/2,
         sub_seconds_from_ts/2]).

timestring_to_timestamp(Tstr) ->
    {TsInt,_} = string:to_integer(Tstr),
    Tstamp = {TsInt div 1000000, TsInt rem 1000000, 0},
    Tstamp.

timestamp_to_timestring(Tstmp) ->
    element(1,Tstmp)*1000000 + element(2,Tstmp).

timestamp_diff(Ts,Te) ->
    % ignore Megaseconds
    Td = (element(2,Te) + element(3,Te)/1000000) - (element(2,Ts) + element(3,Ts)/1000000),
    Td.

add_seconds_to_ts(Ts,Seconds) ->
    NewS = element(2,Ts) + Seconds,
    NewTs = {element(1,Ts) + NewS div 1000000,
             NewS rem 1000000,
             element(3,Ts)},
    NewTs.

sub_seconds_from_ts({MeS,S,MiS},Seconds) when Seconds =< S ->
    {MeS, S-Seconds, MiS};

sub_seconds_from_ts({MeS,S,MiS},Seconds) when Seconds > S ->
    {MeS-1, (S+1000000)-Seconds, MiS};

sub_seconds_from_ts(Ts,Seconds) when Ts >= Seconds ->
    add_seconds_to_ts({0,0,0},Ts-Seconds).
