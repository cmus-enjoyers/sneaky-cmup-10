import gleam/io
import gleam/list
import gleam/string

pub fn repeat_str_length(str: String, symbol: String) -> String {
  string.length(str)
  |> string.repeat(symbol, _)
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
) -> fn(String, Int) -> String {
  let length = list.length(string_list) - 1

  fn(str: String, index: Int) -> String {
    case index {
      0 -> add_top_curled_borders(str)
      index if index == length -> add_bottom_curled_borders(str)
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

  splitted
  |> list.index_map(add_side_borders_list(splitted))
  |> string.join(with: "\n")
}

pub fn print_banner(content: String) -> Nil {
  banner(content) |> io.println
}
