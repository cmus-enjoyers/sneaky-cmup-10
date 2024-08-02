@external(javascript, "./node_fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

@external(javascript, "./node_fs.mjs", "readdirSync")
pub fn ls(path: String) -> Result(List(String), string)
