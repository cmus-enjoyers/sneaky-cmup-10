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
) -> Result(PlaylistWritable, String) {
  fs.ls(playlist.path)
  |> result.map(fn(music) { Playlist(name: playlist.name, tracks: music) })
}

pub fn convert_to_playlist_with_path(playlist: String) -> PlaylistNameWithPath {
  PlaylistNameWithPath(
    name: playlist,
    path: fs.path(music_directory <> "/" <> playlist),
  )
}

pub fn filter_and_convert(playlists: List(String)) -> List(PlaylistNameWithPath) {
  playlists
  |> list.filter(is_non_playlist)
  |> list.map(convert_to_playlist_with_path)
  |> list.map(convert_playlist_with_name_to_playlist_writable)
}

pub fn convert_to_writable_playlists(playlists: List(PlaylistNameWithPath)) {
  playlist |> 
}

pub fn main() {
  fs.path(music_directory)
  |> fs.ls
  |> result.map(filter_and_convert)
  |> result.try(convert_to_writable_playlists)
  |> io.debug
}
