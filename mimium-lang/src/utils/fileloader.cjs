// foo.js
// import fs from "fs";
const fs = require("fs");

export function read_file(path) {
  return fs.readFileSync(path, { encoding: "utf8" });
}

export function get_env(key){
    return process.env[key]
}