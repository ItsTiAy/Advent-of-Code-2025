module Helpers

open System
open System.IO

let read =
    File.ReadAllLines $"{Directory.GetParent(AppContext.BaseDirectory).Parent.Parent.Parent.FullName}/input.txt"

let readDelimiter delimiter = read.[0].Split [| delimiter |]
