import { Ok as GleamOk, Error as GleamError, toList } from "./gleam.mjs";
import {
  writeFileSync as _writeFileSync,
  readdirSync as _readdirSync,
} from "node:fs";
import { normalize } from "node:path";

export const result = (f) => {
  try {
    return new GleamOk(f());
  } catch (e) {
    return new GleamError(e);
  }
};

export const writeFileSync = (path, content) =>
  result(() => _writeFileSync(normalize(path), content));

export const readdirSync = (path) => result(() => toList(_readdirSync(path)));
