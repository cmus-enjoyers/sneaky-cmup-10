import fs
import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import gleam/string

// TODO: Fix error types in Result's

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
  playlist: PlaylistNameWithPath,
) -> PlaylistWritable {
  case
    fs.ls(playlist.path)
    |> result.map(fn(music) { Playlist(name: playlist.name, tracks: music) })
  {
    Ok(value) -> value
    Error(_) -> panic as "Cannot read dir for some reason... :("
  }
}

pub fn convert_to_playlist_with_path(playlist: String) -> PlaylistNameWithPath {
  PlaylistNameWithPath(
    name: playlist,
    path: fs.path(music_directory <> "/" <> playlist),
  )
}

pub fn filter_and_convert(playlists: List(String)) -> List(PlaylistWritable) {
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
