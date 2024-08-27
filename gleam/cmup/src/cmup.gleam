import fs
import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import gleam/string

// TODO: Fix error types in Result's

pub const music_directory = "~/music"

pub const cmus_playlists_directory = "~/.config/cmus/playlists"

pub type PlaylistWritable {
  Playlist(name: String, tracks: List(String))
}

pub type PlaylistNameWithPath {
  PlaylistNameWithPath(name: String, path: String)
}

pub fn is_non_playlist(playlist: String) -> Bool {
  playlist |> string.starts_with(".") |> bool.negate
}

pub fn cmus_music_path(paths: List(String)) {
  fs.join_paths(paths) |> fs.join_path(cmus_playlists_directory, _)
}

pub fn convert_playlist_with_name_to_playlist_writable(
  playlist: PlaylistNameWithPath,
) -> PlaylistWritable {
  case
    fs.ls(playlist.path)
    |> result.map(fn(music) {
      Playlist(
        name: playlist.name,
        tracks: music |> list.map(fn(music) { cmus_music_path([music]) }),
      )
    })
  {
    Ok(value) -> value
    // TODO: Maybe remove this panic
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

pub fn write_playlist(playlist: PlaylistWritable) {
  playlist.tracks |> string.join(with: "\n") |>
  fs.write(fs.join_path(cmus_playlists_directory, playlist.name), _)
}

pub fn write_playlists(
  playlists: List(PlaylistWritable),
) {
  playlists
  |> list.try_map(write_playlist) 
}

pub fn main() {
  case fs.path(music_directory)
  |> fs.ls
  |> result.map(filter_and_convert)
  |> result.try(write_playlists) {
      Ok(_) -> "Successfuly updated playlists :3"
      Error(err) -> err //"Got an unexpected error..."
    } |> io.debug
}
