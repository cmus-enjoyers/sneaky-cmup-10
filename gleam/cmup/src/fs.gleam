import os

@external(javascript, "./node_fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

@external(javascript, "./node_fs.mjs", "readdirSync")
pub fn ls(path: String) -> Result(List(String), string)

/// Fixes nodejs's ~ symbol not recognized
pub fn path(path: String) -> String {
  case path {
    "~" <> path -> os.home() <> path
    _ -> path
  }
}
