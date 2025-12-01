module Helpers

open System
open System.IO

let read =
    File.ReadAllLines $"{Directory.GetParent(AppContext.BaseDirectory).Parent.Parent.Parent.FullName}/input.txt"
