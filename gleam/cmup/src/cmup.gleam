import gleam/io
import utils

@external(javascript, "./fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "~/.config/cmus/playlists/"

pub fn main() {
  io.println(utils.banner("Hello from cmus!"))
}
