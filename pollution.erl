%%%-------------------------------------------------------------------
%%% @author Lenovo Thinkpad X230
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2019 09:17
%%%-------------------------------------------------------------------
-module(pollution).
-author("Lenovo Thinkpad X230").

%% API
%%, removeValue/4, getOneValue/4, getStationMean/3, detDailyMean/3, getAirQualityIndex/3
-export([createMonitor/0, addStation/3, addValue/5]).
-record(elements, {pm25=#{}, pm10=#{}, temperature=#{}}).
-record(station, {stationName, x, y}).

createMonitor() -> #{station=>[]}.

addStation(Station, {X, Y}, M1) ->
  try maps:get({Station, {X, Y}}, M1) of
    _ -> M1
  catch
    error: _ ->
      ExistStation = existStation(Station, M1),
      case(ExistStation) of
        true ->
          ExistPosition = existPosition({X, Y}, M1),
          case(ExistPosition) of
            true ->
              Stations = maps:get(station, M1),
              M1#{#station{stationName = Station, x = X, y = Y} => #elements{},
                station := [#station{stationName = Station, x = X, y = Y} | Stations]};
            _ -> M1
          end;
      _ -> M1
      end
  end.

existStation(Station, M1) ->
  F = fun(Key, _) -> case(Key) of
                       #station{} -> Key#station.stationName == Station;
                       _ -> false
                     end
      end,
  M2 = maps:filter(F, M1),
  M2 == #{}.

existPosition({X, Y}, M1) ->
  F = fun(Key, _) -> case(Key) of
                       #station{} -> Key#station.x == X andalso Key#station.y == Y;
                       _ -> false
                     end
      end,
  M2 = maps:filter(F, M1),
  M2 == #{}.

%addValue(StationDetails, Time, Element, Value, M1)->
% case(is_tuple(StationDetails)) of
%   true -> {X, Y} = StationDetails,
%   StationKey = {_, {X, Y}};
%   false -> StationName = StationDetails,
%     StationKey = {StationName, _}
% end,
% ContainsStation = maps:is_key(StationKey, M1#station),
% case(ContainsStation) of
%   true ->
%
% end
