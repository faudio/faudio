

var ffi = require('ffi')

var ae_ = ffi.Library('libae', {
    'doremir_audio_engine_version':         [ 'pointer', [] ],
    'doremir_audio_engine_version_string':  [ 'pointer', [] ],
    'doremir_string_to_utf8':               [ 'string', ['pointer'] ],
})
var ae = {
    version         : ae_.doremir_audio_engine_version,
    versionString   : ae_.doremir_audio_engine_version_string,
    string : {
        toUtf8      : ae_.doremir_string_to_utf8,
    }
}


var v  = ae.versionString();
var v2 = ae.string.toUtf8(v);

console.log(v2);

