import gleam/list
import gleam/string

pub fn repeat_str_length(str: String, symbol: String) -> String {
  string.length(str)
  |> string.repeat(symbol, _)
}

pub fn banner(content: String) -> String {
  let line = repeat_str_length(content, "─")
  let bottom_border = "\n" <> line
  let top_border = line <> "\n"
  let splitted = string.split(top_border <> content <> bottom_border, on: "\n")
  let total_length = list.length(splitted) - 1

  splitted
  |> list.index_map(fn(str, index) {
    case index {
      0 -> "╭" <> str <> "╮"
      total_length -> "╰" <> str <> "╯"
      _ -> "│ " <> str <> " │"
    }
  })
  |> string.join(with: "\n")
}
