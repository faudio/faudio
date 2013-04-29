
var ae      = require('./');
var dev     = ae.device;
var audio   = ae.device.audio;

console.log(ae.version());
console.log(ae.versionString());
ae.setLogStd();

ae.withEngine(function() {
    audio.withSession(function(audioSession) {

        console.log(audioSession);

        var devices = audioSession.devices();
        // console.log(devices);
        devices.forEach(function (d) {
            console.log("    " + d);
            console.log("    " + d.toString());
            console.log("    " + d.name());
            console.log("    " + d.hostName());
            console.log("    " + d.hasInput());
            console.log("    " + d.hasOutput());
        });
    })
})

