
var ffi      = require('ffi');
var cliColor = require('cli-color');

var fae_ = ffi.Library('libfae', {
    'fae_audio_engine_version':             ['pointer', [] ],
    'fae_audio_engine_version_string':      ['pointer', [] ],
    'fae_string_to_utf8':                   ['string',  ['pointer'] ],
                                                
    'fae_audio_engine_initialize':          ['void',    [] ],
    'fae_audio_engine_terminate':           ['void',    [] ],
                                                
    'fae_audio_engine_set_log':             ['void',    ['pointer','pointer']],
    'fae_audio_engine_set_log_std':         ['void',    []],
                                                
                                                
    'fae_list_empty':                       ['pointer', []],
    'fae_list_cons':                        ['pointer', ['pointer','pointer']],
    'fae_list_is_empty':                    ['bool',    ['pointer']],
    'fae_list_head':                        ['pointer', ['pointer']],
    'fae_list_tail':                        ['pointer', ['pointer']],
                                                
    'fae_dynamic_get_type':                 ['int',     ['pointer']],
                                                
    'fae_pair_create':                      ['pointer', ['pointer', 'pointer']],
    'fae_pair_fst':                         ['pointer', ['pointer']],
    'fae_pair_snd':                         ['pointer', ['pointer']],    
                                                
    'fae_to_bool':                          ['bool',    ['pointer']],
    'fae_to_int8':                          ['int',     ['pointer']],
    'fae_to_int16':                         ['int',     ['pointer']],
    'fae_peek_int32':                       ['int',     ['pointer']], 
    'fae_peek_int64':                       ['int',     ['pointer']], 
    'fae_peek_float':                       ['double',  ['pointer']], 
    'fae_peek_double':                      ['double',  ['pointer']], 
                                                
    'fae_device_audio_begin_session':       ['pointer', []],
    'fae_device_audio_end_session':         ['void',    ['pointer']],
    'fae_device_audio_with_session':        ['void',    ['pointer','pointer','pointer','pointer']],
    'fae_device_audio_all':                 ['pointer', ['pointer']],
    'fae_device_audio_default':             ['pointer', ['pointer']],
    'fae_device_audio_set_status_callback': ['pointer', ['pointer']],
    'fae_device_audio_name':                ['pointer', ['pointer']],
    'fae_device_audio_host_name':           ['pointer', ['pointer']],
    'fae_device_audio_has_input':           ['bool',    ['pointer']],
    'fae_device_audio_has_output':          ['bool',    ['pointer']],
    'fae_device_audio_input_type':          ['pointer', ['pointer']],
    'fae_device_audio_output_type':         ['pointer', ['pointer']],
    'fae_device_audio_open_stream':         ['pointer', ['pointer','pointer','pointer']],
    'fae_device_audio_close_stream':        ['void',    ['pointer']],
    'fae_device_audio_with_stream':         ['void',    ['pointer','pointer','pointer','pointer','pointer','pointer','pointer']],
    
    'fae_error_check':                      ['bool',    ['pointer']],
    'fae_error_message':                    ['pointer', ['pointer']],
    'fae_string_show':                      ['pointer', ['pointer']],

    'fae_type_channels':                    ['int',     ['pointer']],
})

var bool_ = fae_.fae_to_bool;
var i8_   = fae_.fae_to_int8;
var i16_  = fae_.fae_to_int16;
var i32_  = fae_.fae_peek_int32;
var i64_  = fae_.fae_peek_int64;
var f32_  = fae_.fae_peek_float;
var f64_  = fae_.fae_peek_double;

var pair_ = function (a, importFun) {
    var imp = importFun || dyn_;
    var x   = fae_.fae_pair_fst(a); 
    var y   = fae_.fae_pair_snd(a);
    return [imp(x),imp(y)];
}

var list_   = function(as, importFun) {
    var imp = importFun || dyn_;
    if (fae_.fae_list_is_empty(as)) {
        return [];
    } else {
        var b  = fae_.fae_list_head(as);
        var bs = fae_.fae_list_tail(as);
        return [imp(b)].concat(list_(bs, importFun));
    }
}

var string_     = fae_.fae_string_to_utf8;
var check_      = fae_.fae_error_check;
var message_    = fae_.fae_error_message;
var show_       = fae_.fae_string_show;

var dyn_ = function(a) {
    var t = fae_.fae_dynamic_get_type(a);
    switch (t) {
        case 0:     return bool_(a);
        case 1:     return i8_(a);
        case 2:     return i16_(a);
        case 3:     return i32_(a);
        case 4:     return i64_(a);
        case 5:     return f32_(a);
        case 6:     return f64_(a);
        case 7:     return pair_(a);
        case 8:     return list_(a);
        // case 9:     return set_(a);
        // case 10:    return map_(a);
        case 11:    return string_(a);
        // case 12:    return ratio_(a);
        default:    throw "dyn_: Missing case";
    }
}         

var id   = function(a)       { return a };
var o    = function(g, f)    { return function (a) { return g(f(a))     } }
var o3   = function(g, f, h) { return function (a) { return g(f(h(a)))  } }
var get  = function(n)       { return function (a) { return a[n]        } }
var make = function(c)       { return function (a) { return new c(a)    } }


var errorStyle = cliColor.red.bold;

var thowIfErr = function (a) {
    if (check_(a)) {
        var m = string_(message_(a));
        throw errorStyle("Error: ") + m;
    } else {
        return a;
    }
}

var showable = function(a) {
    a.inspect  = function() { return string_(show_(this.value)) };
    a.toString = a.inspect;
    return a;
}         

var AudioSession = function (a) { this.value = a; showable(this) }
AudioSession.prototype.end              = function () { return fae.device.audio.endSession(this) }
AudioSession.prototype.devices          = function () { return fae.device.audio.devices(this) }
AudioSession.prototype.defaultDevices   = function () { return fae.device.audio.defaultDevices(this) }

var AudioDevice  = function (a) { this.value = a; showable(this) }
AudioDevice.prototype.name              = function () { return fae.device.audio.name(this) }
AudioDevice.prototype.hostName          = function () { return fae.device.audio.hostName(this) }
AudioDevice.prototype.hasInput          = function () { return fae.device.audio.hasInput(this) }
AudioDevice.prototype.hasOutput         = function () { return fae.device.audio.hasOutput(this) }
AudioDevice.prototype.inputType         = function () { return fae.device.audio.inputType(this) }
AudioDevice.prototype.outputType        = function () { return fae.device.audio.outputType(this) }

var AudioType    = function (a) { this.value = a; showable(this) }
AudioType.prototype.channels            = function () { return fae.type.channels(this) }

var fae = {
    version                     : function() { return dyn_(fae_.fae_audio_engine_version() )},
    versionString               : function() { return string_(fae_.fae_audio_engine_version_string() )} ,
    setLog                      : fae_.fae_audio_engine_set_log,
    setLogStd                   : fae_.fae_audio_engine_set_log_std,
    initialize                  : fae_.fae_audio_engine_initialize,
    terminate                   : fae_.fae_audio_engine_terminate,
    withEngine                  : function(f) { fae.initialize(); f(); fae.terminate(); },

    type : {
        channels                : function(s) { return fae_.fae_type_channels(s.value) },
    },

    device : {
        audio : {
            beginSession        : function () { return o(make(AudioSession), thowIfErr)(fae_.fae_device_audio_begin_session()) },
            endSession          : o(fae_.fae_device_audio_end_session, get("value")),
            withSession         : function(f) { var s = fae.device.audio.beginSession(); f(s); fae.device.audio.endSession(s); },
            devices             : function (s) { return list_(fae_.fae_device_audio_all(s.value), make(AudioDevice))},
            defaultDevices      : function (s) { return pair_(fae_.fae_device_audio_default(s.value), make(AudioDevice))},
            setStatusCallback   : fae_.fae_device_audio_set_status_callback,
            name                : o3(string_,         fae_.fae_device_audio_name,        get("value")),
            hostName            : o3(string_,         fae_.fae_device_audio_host_name,   get("value")),
            hasInput            : o3(id,              fae_.fae_device_audio_has_input,   get("value")),
            hasOutput           : o3(id,              fae_.fae_device_audio_has_output,  get("value")),
            inputType           : o3(make(AudioType), fae_.fae_device_audio_input_type,  get("value")),
            outputType          : o3(make(AudioType), fae_.fae_device_audio_output_type, get("value")),
            openStream          : fae_.fae_device_audio_open_stream,
            closeStream         : fae_.fae_device_audio_close_stream,
            withStream          : fae_.fae_device_audio_with_stream,
            Session             : AudioSession,
            Device              : AudioDevice,
            Type                : AudioType,
        }
    },
    
}

module.exports = fae;
