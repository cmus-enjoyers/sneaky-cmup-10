import { Ok as GleamOk, Error as GleamError } from "./gleam.mjs";
import { writeFileSync as _writeFileSync } from "node:fs";
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
