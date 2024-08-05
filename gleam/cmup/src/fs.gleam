import os
import gleam/string

// TODO: Fix error types in Result's

@external(javascript, "./node_fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

@external(javascript, "./node_fs.mjs", "readdirSync")
pub fn ls(path: String) -> Result(List(String), String)

/// Fixes nodejs's ~ symbol not recognized
pub fn path(path: String) -> String {
  case path {
    "~" <> path -> os.home() <> path
    _ -> path
  }
}

pub fn joinPath(paths: List(String)) -> String {
  string.join(paths, with: "/") |> path
}
