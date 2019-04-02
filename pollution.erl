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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).
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

addValue(StationDetails, Time, Element, Value, M1)->
  Util = maps:get(station, M1),
  case (is_tuple(StationDetails)) of
  true -> Stations = getNameByPosition(Util, StationDetails);
  false -> Stations = getName(Util, StationDetails)
  end,
  Measurements = maps:get(Stations, M1),
  OldElementMeasurements = getElement(Element, Measurements),
  NewElementMeasurements = OldElementMeasurements#{Time => Value},
  UpdatedMeas = addElement(Measurements, NewElementMeasurements, Element),
  M1#{Stations => UpdatedMeas}.

removeValue(StationDetails, Time, Element, M1) ->
  Util = maps:get(station, M1),
  case (is_tuple(StationDetails)) of
    true -> Stations = getNameByPosition(Util, StationDetails);
    false -> Stations = getName(Util, StationDetails)
  end,
  Measurements = maps:get(Stations, M1),
  OldElementMeasurements = getElement(Element, Measurements),
  NewElementMeasurements = maps:remove(Time, OldElementMeasurements),
  UpdatedMeas = addElement(Measurements, NewElementMeasurements, Element),
  M1#{Stations => UpdatedMeas}.

getOneValue(StationDetails, Time, Element, M1) ->
  Util = maps:get(station, M1),
  case (is_tuple(StationDetails)) of
    true -> Stations = getNameByPosition(Util, StationDetails);
    false -> Stations = getName(Util, StationDetails)
  end,
  Measurements = maps:get(Stations, M1),
  ElementMeasurements = getElement(Element, Measurements),
  maps:get(Time, ElementMeasurements).

getStationMean(StationDetails, Element, M1) ->
  Util = maps:get(station, M1),
  case (is_tuple(StationDetails)) of
    true -> Stations = getNameByPosition(Util, StationDetails);
    false -> Stations = getName(Util, StationDetails)
  end,
  Measurements = maps:get(Stations, M1),
  ElementMeas = getElement(Element, Measurements),
  FinalMeas = maps:value(ElementMeas),
  mean(FinalMeas, 0, 0).

getDailyMean(Element, Time, M1) ->
  Values = maps:values(M1),
  case Element of
    "PM10" -> ElemValues = [X#elements.pm10 ||  X <- Values, is_record(X,elements)];
    "PM2,5" -> ElemValues = [X#elements.pm25 || X <- Values, is_record(X,elements)];
    "temperature" -> ElemValues = [X#elements.temperature || X <- Values, is_record(X,elements)]
  end,
  ElemDayValues = [filterTime(Time, X) || X <- ElemValues],
  ValuesList = [maps:values(X) || X <- ElemDayValues],
  EValues = lists:foldl(fun(X, List) -> X ++ List end, [], ValuesList),
  mean(EValues, 0, 0).

%all Utility functions used in the program
mean([], _, _) -> 0;
mean([H | T], Count, Acc) -> mean(T, Count + 1, Acc + H).

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

getNameByPosition([], {_, _}) -> [];
getNameByPosition([H = {station, _, X, Y} | T], {X, Y}) -> H;
getNameByPosition([_ | T], {X, Y}) -> getNameByPosition(T, {X, Y}).

getName([[] | _], _) -> [];
getName([H = #station{} | _], StationName) when (H#station.stationName == StationName) -> H;
getName([_ | T], StationName) -> getName(T, StationName).

getElement(Element, Measurements) ->
case Element of
    "PM10" -> Measurements#elements.pm10;
    "PM2,5" -> Measurements#elements.pm25;
    "temperature" -> Measurements#elements.temperature
  end.

addElement(OldMap, NewMap, Element) ->
  case Element of
    "PM10" -> OldMap#elements{pm10 = NewMap};
    "PM2,5" -> OldMap#elements{pm25 = NewMap};
    "temperature" -> OldMap#elements{pm25 = NewMap}
  end.


filterTime(Time, M1) ->
  Fun = fun(X,_) when (X == Time)-> true,
      fun(_, _) -> false
    end
        end,
  maps:filter(Fun, M1).
