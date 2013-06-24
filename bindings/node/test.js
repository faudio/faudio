
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




/*
Generators
    Noise
    Sin, Square, Saw, Tri
    Impulses
Analyzers
    ?
    level(peak/rms)
Util
    Zip, Unzip, Mag2Db, Integral, Diff


I/O
    Input, output          
    Buffer input, buffer output
    File input, file output

Filters
    Delay             
    Delay (interpolating allpass/linear)
    Biquad (pass, notch, shelves)
    Resonant?
Reverb
    FreeVerb, GVerb
Dynamics           
    Normalise
    Envelope
    Gate, Compress
Pitch, spectral
    FFT, IFFT, RFFT
Memoize/Wavetables
Space
    Stereo pan
*/





// TODO functions of freq
var A           = pow(10, dbGain /40);
var omega       = 2 * Math.PI * freq /srate;
var cos(omega)  = cos(omega);
var alpha       = sin(omega) * sinh(Math.LN2 /2 * bandwidth * omega / sin(omega) );
var beta        = sqrt(A + A);

var lpf = function(x) {
    b0 = (1 - cos(omega)) /2;
    b1 = 1 - cos(omega);
    b2 = (1 - cos(omega)) /2;
    a0 = 1 + alpha;
    a1 = -2 * cos(omega);
    a2 = 1 - alpha;
    return biquad(x,a1,a2,b0,b1,b2);
}

var hpf = function(x) {
    b0 = (1 + cos(omega)) /2;
    b1 = -(1 + cos(omega));
    b2 = (1 + cos(omega)) /2;
    a0 = 1 + alpha;
    a1 = -2 * cos(omega);
    a2 = 1 - alpha;
    return biquad(x,a1,a2,b0,b1,b2);
}
var bpf = function(x) {
    b0 = alpha;
    b1 = 0;
    b2 = -alpha;
    a0 = 1 + alpha;
    a1 = -2 * cos(omega);
    a2 = 1 - alpha;
    return biquad(x,a1,a2,b0,b1,b2);
}
var notch = function(x) {
    b0 = 1;
    b1 = -2 * cos(omega);
    b2 = 1;
    a0 = 1 + alpha;
    a1 = -2 * cos(omega);
    a2 = 1 - alpha;
    return biquad(x,a1,a2,b0,b1,b2);
}
var peq = function(x) {
    b0 = 1 + (alpha * A);
    b1 = -2 * cos(omega);
    b2 = 1 - (alpha * A);
    a0 = 1 + (alpha /A);
    a1 = -2 * cos(omega);
    a2 = 1 - (alpha /A);
    return biquad(x,a1,a2,b0,b1,b2);
}
var lsh = function(x) {
    b0 = A * ((A + 1) - (A - 1) * cos(omega) + beta * sin(omega));
    b1 = 2 * A * ((A - 1) - (A + 1) * cos(omega));
    b2 = A * ((A + 1) - (A - 1) * cos(omega) - beta * sin(omega));
    a0 = (A + 1) + (A - 1) * cos(omega) + beta * sin(omega);
    a1 = -2 * ((A - 1) + (A + 1) * cos(omega));
    a2 = (A + 1) + (A - 1) * cos(omega) - beta * sin(omega);
    return biquad(x,a1,a2,b0,b1,b2);
}
var hsh = function(x) {
    b0 = A * ((A + 1) + (A - 1) * cos(omega) + beta * sin(omega));
    b1 = -2 * A * ((A - 1) + (A + 1) * cos(omega));
    b2 = A * ((A + 1) + (A - 1) * cos(omega) - beta * sin(omega));
    a0 = (A + 1) - (A - 1) * cos(omega) + beta * sin(omega);
    a1 = 2 * ((A - 1) - (A + 1) * cos(omega));
    a2 = (A + 1) - (A - 1) * cos(omega) - beta * sin(omega);
    return biquad(x,a1,a2,b0,b1,b2);
}

var biquad = function (x, a1, a2, b0, b1, b2) {
    return fix(function(y) {
        return (x          * b0) 
            +  (delay(1,x) * b1 + delay(1,y) * (-a1)) 
            +  (delay(2,x) * b2 + delay(2,y) * (-a2));
    });
}

var comb = function (x) {
    return a * x + b * x - (delay    ) + cyn-(delay) 
}
