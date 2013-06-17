
var fae     = require('./');
var dev     = fae.device;
var audio   = fae.device.audio;

console.log(fae.version());
console.log(fae.versionString());
fae.setLogStd();

fae.withEngine(function() {
    audio.withSession(function(session) {

        console.log("Instance: " + String(session instanceof audio.Session))
        console.log(session);

        var devices = session.devices();
        console.log(session.defaultDevices());
        console.log(devices);
        
        devices.forEach(function (d) {
            console.log("    " + d);
            console.log("    " + d.toString());
            console.log("    " + d.name());
            console.log("    " + d.hostName());
            console.log("    " + d.hasInput());
            console.log("    " + d.hasOutput());
            console.log("    " + d.inputType().channels());
            console.log("    " + d.outputType().channels());
        });
    })
})

