import gleam/io
import utils

@external(javascript, "./fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

const music_directory = "~/music"
const cmus_playlists_directory = "~/.config/cmus/playlists/"

pub fn main() {
  io.println("Hello from cmup!")

  let result = case write_file("./hello.js", "console.log('hello world')") {
    Ok(_) -> "File Written!"
    Error(_) -> "Error occured!"
  }

  io.debug(result)
  io.debug(music_directory)

  io.debug(utils.banner("Hello!"))
} 
