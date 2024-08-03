import fs
import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import gleam/string

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "~/.config/cmus/playlists/"

pub type Playlists {
  // TODO: you know what to do here.
}

pub fn is_non_playlist(playlist: String) -> Bool {
  playlist |> string.starts_with(".") |> bool.negate
}

pub fn convert_to_full_path(playlist: String) {
  fs.path(music_directory <> "/" <> playlist)
}

pub fn filter_and_convert(playlists: List(String)) -> String {
  playlists |> list.filter(is_non_playlist) |> list.map(convert_to_full_path)
}

pub fn main() {
  fs.path(music_directory)
  |> fs.ls
  |> result.map(filter_and_convert)
  |> io.debug
}
