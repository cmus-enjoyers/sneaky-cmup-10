import utils

@external(javascript, "./fs.mjs", "writeFileSync")
pub fn write_file(path: String, content: String) -> Result(string, string)

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "~/.config/cmus/playlists/"

pub fn main() {
  utils.print_banner("Hello from cmus! \n hello!")
}
