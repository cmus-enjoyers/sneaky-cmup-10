import gleam/string
import gleam/list

pub fn repeat_str_length(str: String, symbol: String) -> String {
  string.length(str) |> string.repeat(symbol, _)
}

fn add_side_borders(total_length: Int, str: String, index: Int) -> String {
  case index {
    total_length -> "└" <> str <> "┘"
  }
}

pub fn banner(content: String) -> String {
  let bottom_border = repeat_str_length(content, "─")
  let top_border = repeat_str_length(content, "─")
  let splitted = string.split(content, on: "\n")
  let border = add_side_borders(list.length(splitted), _, )

  splitted |> list.map(border)
}
