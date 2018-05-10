var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
var binVersion = packageInfo.version;

var root = "https://github.com/cmditch/elm-ethereum-generator/releases/download/" +
  binVersion +
  "/elm-ethereum-generator-" +
  binVersion;

module.exports = binwrap({
  binaries: ["elm-ethereum-generator"],
  urls: {
    "darwin-x64": root + "-mac-x64.tgz"
  }
});