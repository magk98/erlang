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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getAirQualityIndex/3, tryProgram/0]).
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
  ElemDayValues = [filterTimeByDay(Time, X) || X <- ElemValues],
  ValuesList = [maps:values(X) || X <- ElemDayValues],
  EValues = lists:foldl(fun(X, List) -> X ++ List end, [], ValuesList),
  mean(EValues, 0, 0).

getAirQualityIndex(StationDetails, Time, M1) ->
  LimitPM10 = 50,
  LimitPM25 = 30,
  PM10Value = getOneValue(StationDetails, Time, "PM10", M1),
  PM25Value = getOneValue(StationDetails, Time, "PM2,5", M1),
  max((PM10Value/LimitPM10) * 100, (PM25Value/LimitPM25) * 100).

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
getNameByPosition([H = {station, _, X, Y} | _], {X, Y}) -> H;
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

filterTimeByDay(Day, M1) ->
  Fun = fun(X,_) ->
    case X of
      {Day, {_, _, _}} -> true;
      _ -> false
    end
        end,
  maps:filter(Fun, M1).

tryProgram() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Słowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, calendar:local_time(), "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Słowackiego", {{2019,2,15},{12,34,33}}, "PM10", 122, P2),
  P4 = pollution:addValue("Aleja Słowackiego", calendar:local_time(), "PM10", 13, P3),
  P5 = pollution:addStation("Manhattan", {50, 18.45}, P4),
  P6 = pollution:addValue("Manhattan",{{2019,2,15},{12,34,33}} , "PM10", 200, P5),
  P7 = pollution:addValue("Manhattan",{{2019,2,15},{12,34,33}} , "PM2,5", 213, P6),
  getAirQualityIndex("Manhattan",{{2019,2,15},{12,34,33}}, P7).
