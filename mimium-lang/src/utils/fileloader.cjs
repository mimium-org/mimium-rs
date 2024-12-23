// foo.js
// import fs from "fs";
const fs = require("fs");

function read_file(path) {
  return fs.readFileSync(path, { encoding: "utf8" });
}
exports.read_file = read_file;
function get_env(key){
    return process.env[key]
}
exports.get_env = get_env;