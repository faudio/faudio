
var ae = require('./');

console.log(ae.version());
console.log(ae.versionString());

ae.setLogStd();
ae.initialize();

var s = ae.device.audio.beginSession();
ae.device.audio.endSession(s);

var s2 = ae.device.audio.beginSession();
ae.device.audio.endSession(s2);



ae.terminate();

