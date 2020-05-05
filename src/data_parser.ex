defmodule DataParser do
  @moduledoc """
  Module parsing measurements data from csv files to server.
  Each line like:
  DATE,HOUR,LONGITUDE,LATITUDE,VALUE
  """
  def parseLine(line) do
    [date, time, lng, lat, value] = String.split(line, ",")
    %{:datetime =>
      {
        date
        |> String.split("-")
        |> Enum.map(&String.to_integer/1)
        |> Enum.reverse
        |> List.to_tuple,
        time
        |> String.split(":")
        |> Enum.map(&String.to_integer/1)
        |> List.insert_at(2, 0)
        |> List.to_tuple
    },
      :location =>
        {lng |> Float.parse |> elem(0), lat |> String.to_float},
      :pollutionLevel  => value |> String.to_integer
    }
  end

  def importLinesFromCSV(path) do
    measurements = path |> File.read! |> String.split("\r\n") |> Enum.map(fn e -> Map.put(parseLine(e), :type, 'PM10') end)
    measurements |> uniqueStations |> Enum.each(&addStation/1)
    measurements |> Enum.each(&addMeasurement/1)
  end

  def uniqueStations(measurements) do
    measurements |>
      Enum.uniq_by(fn %{:location => location} -> location end) |>
      Enum.map(fn %{:location => location = {lng, lat}} -> {'station_#{lng}_#{lat}', location} end)
  end

  def addStation({name, location}) do
    case :pollution_gen_server.addStation(name, location) do
      {:error, value} -> IO.puts(value)
      _ -> IO.puts("stations added")
    end
  end

  def addMeasurement(%{:datetime => datetime, :location => location, :pollutionLevel => value, :type => type}) do
    case :pollution_gen_server.addValue(location, datetime, type, value) do
      {:error, value} -> IO.puts(value)
      _ -> IO.puts("measurement added")
    end
  end

end