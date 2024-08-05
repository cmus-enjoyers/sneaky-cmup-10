import gleam/string
import gleam/list
import os

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

/// This function expects that `a` String will end with `/`
fn join_with_slash_at_start(a: String, b: String) {
  case b {
    "/" <> b -> a <> string.crop(b, from: "/")
    _ -> a <> b
  }
}

// This function expects that `a` String will not end with `/`
fn join_without_slash_at_start(a: String, b: String) -> String {
  case b {
    "/" <> b -> a <> b
    _ -> a <> "/" <> b
  }
}

fn join_path(a: String, b: String) -> String {
  case a {
    a <> "/" -> join_with_slash_at_start(a, b)
    _ -> join_with_slash_at_start(a, b)
  }
}

pub fn join_paths(paths: List(String)) -> String {
  string.join(paths, with: "/") |> path

  paths |> list.fold(0, fn join_path)
}
