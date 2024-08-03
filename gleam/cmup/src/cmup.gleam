import fs
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/bool
import utils

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "~/.config/cmus/playlists/"

pub fn remove_non_playlists(playlist: String) -> Bool {
  playlist |> string.starts_with(".") |> bool.negate
}

pub fn main() {
  fs.path(music_directory)
  |> fs.ls
  |> result.map(fn(value) { list.filter(value, remove_non_playlists) }) |>
  io.debug
}
