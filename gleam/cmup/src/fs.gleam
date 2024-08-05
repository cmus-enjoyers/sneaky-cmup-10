import gleam/string
import os

// TODO: Fix error types in Result's

@external(javascript, "./node_fs.mjs", "writeFileSync")
pub fn write(path: String, content: String) -> Result(string, string)

@external(javascript, "./node_fs.mjs", "readdirSync")
pub fn ls(path: String) -> Result(List(String), String)

/// Fixes nodejs's ~ symbol not recognized
pub fn path(path: String) -> String {
  case path {
    "~" <> path -> os.home() <> path
    _ -> path
  }
}

pub fn join_path(a: String, b: String) -> String {
  a <> "/" <> b
}

pub fn join_paths(paths: List(String)) -> String {
  string.join(paths, with: "/") |> path
}
