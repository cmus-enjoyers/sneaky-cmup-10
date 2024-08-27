import { Ok as GleamOk, Error as GleamError, toList } from "./gleam.mjs";
import {
  writeFileSync as _writeFileSync,
  readdirSync as _readdirSync,
  readFileSync as _readFileSync,
} from "node:fs";
import { normalize } from "node:path";

export const tryCatch = (f) => {
  try {
    return new GleamOk(f());
  } catch (e) {
    return new GleamError(e);
  }
};

export const writeFileSync = (path, content) =>
  tryCatch(() => _writeFileSync(normalize(path), content));

export const readdirSync = (path) => tryCatch(() => toList(_readdirSync(path)));
