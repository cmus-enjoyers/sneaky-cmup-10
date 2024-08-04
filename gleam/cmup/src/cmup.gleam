import fs
import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import gleam/string

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "~/.config/cmus/playlists/"

pub type PlaylistWritable {
  Playlist(name: String, tracks: List(String))
}

pub type PlaylistNameWithPath {
  PlaylistNameWithPath(name: String, path: String)
}

pub fn is_non_playlist(playlist: String) -> Bool {
  playlist |> string.starts_with(".") |> bool.negate
}

pub fn convert_playlist_with_name_to_playlist_writable(
  thing: PlaylistNameWithPath,
) -> Result(PlaylistWritable) {
  io.debug(fs.ls(thing.path))

  Playlist(name: thing.name, tracks: ["track"])
}

pub fn convert_to_playlist_with_path(playlist: String) -> PlaylistNameWithPath {
  PlaylistNameWithPath(
    name: playlist,
    path: fs.path(music_directory <> "/" <> playlist),
  )
}

pub fn filter_and_convert(playlists: List(String)) {
  playlists
  |> list.filter(is_non_playlist)
  |> list.map(convert_to_playlist_with_path)
  |> list.map(convert_playlist_with_name_to_playlist_writable)
}

pub fn main() {
  fs.path(music_directory)
  |> fs.ls
  |> result.map(filter_and_convert)
  |> io.debug
}
