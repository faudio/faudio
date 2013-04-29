
var ae = require('./');

console.log(ae.version());
console.log(ae.versionString());

ae.setLogStd();
ae.initialize();

var s = ae.device.audio.beginSession();
var devs = ae.device.audio.all(s);

console.log(devs);
console.log(devs[0]);
console.log(devs[0].toString());

ae.device.audio.endSession(s);




ae.terminate();

