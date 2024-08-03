import utils
import gleam/io
import fs
import gleam/result

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "/home/vktrenokh/.config/cmus/playlists/"

pub fn main() {
  // fs.ls(cmus_playlists_directory) |> result.map(io.debug)

  fs.path(music_directory) |> io.debug
}
