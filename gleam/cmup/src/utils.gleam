import gleam/list
import gleam/string
import gleam/int
import gleam/io

pub fn repeat_str_length(str: String, symbol: String) -> String {
  string.length(str)
  |> string.repeat(symbol, _)
}

pub fn banner(content: String) -> String {
  let line = repeat_str_length(content, "─")
  let bottom_border = "\n" <> line
  let top_border = line <> "\n"
  let splitted = string.split(top_border <> content <> bottom_border, on: "\n")
  let length = list.length(splitted) - 1

  splitted
  |> list.index_map(fn(str, index) {
    case index {
      0 -> "╭" <> str <> "╮"
      index if index == length -> "╰" <> str <> "╯"
      _ -> "│ " <> str <> " │"
    }
  })
  |> string.join(with: "\n")
}

pub fn print_banner(content: String) -> Nil {
  banner(content) |> io.println(_)
}
