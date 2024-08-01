import gleam/io

@external(javascript, "./fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

pub fn main() {
  io.println("Hello from cmup!")

  let result = case write_file("./hello.js", "console.log('hello world')") {
    Ok(_) -> "File Written!"
    Error(_) -> "Error occured!"
  }

  io.debug(result)
} 
