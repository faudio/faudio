
var ae = require('./');

var v  = ae.versionString();
var v2 = ae.string.toUtf8(v);

console.log(v2);
console.log("Hello!");