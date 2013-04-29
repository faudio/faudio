
var ae = require('./');

console.log(ae.version());
console.log(ae.versionString());

ae.setLogStd();
ae.initialize();

var sess = ae.device.audio.beginSession();
console.log(sess);
var devs = ae.device.audio.all(sess);

console.log(devs);
console.log(devs[0]);
console.log(devs[0].toString());

ae.device.audio.endSession(sess);




ae.terminate();

