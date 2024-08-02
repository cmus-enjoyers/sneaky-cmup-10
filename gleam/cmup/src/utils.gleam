import gleam/io
import gleam/list
import gleam/string

pub fn get_longest_string_length(strings: List(String)) -> Int {
  strings
  |> list.map(string.length)
  |> list.fold(0, fn(length, acc) {
    case length {
      length if length > acc -> length
      _ -> acc
    }
  })
}

pub fn repeat_str_length(str: String, symbol: String) -> String {
  string.length(str)
  |> string.repeat(symbol, _)
}

pub fn add_missing_spaces(text str: String, to length: Int) {
  let true_length = length - string.length(str)

  str <> string.repeat(" ", true_length)
}

pub fn add_top_curled_borders(string: String) {
  "╭" <> string <> "╮"
}

pub fn add_bottom_curled_borders(string: String) {
  "╰" <> string <> "╯"
}

pub fn add_side_borders(string: String) {
  "│ " <> string <> " │"
}

pub fn add_side_borders_list(
  string_list: List(String),
  string_length: Int,
) -> fn(String, Int) -> String {
  let length = list.length(string_list) - 1

  fn(str: String, index: Int) -> String {
    case index {
      0 -> add_top_curled_borders(str)
      index if index == length ->
        str
        |> add_missing_spaces(to: string_length)
        |> add_bottom_curled_borders
      _ -> add_side_borders(str)
    }
  }
}

pub fn banner(content: String) -> String {
  let line = repeat_str_length(content, "─")
  let bottom_border = "\n" <> line
  let top_border = line <> "\n"
  let splitted = string.split(top_border <> content <> bottom_border, on: "\n")
  let length = list.length(splitted) - 1

  get_longest_string_length(splitted) |> io.debug

  splitted
  |> list.index_map(
    string.length(content) |> add_side_borders_list(splitted, _),
  )
  |> string.join(with: "\n")
}

pub fn print_banner(content: String) -> Nil {
  banner(content) |> io.println
}
