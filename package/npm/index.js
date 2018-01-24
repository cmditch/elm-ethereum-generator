var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
var binVersion = packageInfo.version.replace(/\.[0-9]*$/, "");

var root = "https://github.com/cmditch/elm-web3-contract/releases/download/" +
  binVersion +
  "/elm-web3-contract-" +
  binVersion;

module.exports = binwrap({
  binaries: ["elm-web3-contract"],
  urls: {
    "darwin-x64": root + "-mac-x64.tgz"
  }
});