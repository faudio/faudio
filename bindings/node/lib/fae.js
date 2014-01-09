
var ffi      = require('ffi');
var cliColor = require('cli-color');

// FIXME path
var fa_ = ffi.Library('build/Frameworks/Faudio.framework/Faudio', {
    'fa_audio_engine_version':             ['pointer', [] ],
    'fa_audio_engine_version_string':      ['pointer', [] ],
    'fa_string_to_utf8':                   ['string',  ['pointer'] ],
                                                
    'fa_audio_engine_initialize':          ['void',    [] ],
    'fa_audio_engine_terminate':           ['void',    [] ],
                                                
    'fa_audio_engine_set_log':             ['void',    ['pointer','pointer']],
    'fa_audio_engine_set_log_std':         ['void',    []],
                                                
                                                
    'fa_list_empty':                       ['pointer', []],
    'fa_list_cons':                        ['pointer', ['pointer','pointer']],
    'fa_list_is_empty':                    ['bool',    ['pointer']],
    'fa_list_head':                        ['pointer', ['pointer']],
    'fa_list_tail':                        ['pointer', ['pointer']],
                                                
    'fa_dynamic_get_type':                 ['int',     ['pointer']],
                                                
    'fa_pair_create':                      ['pointer', ['pointer', 'pointer']],
    'fa_pair_fst':                         ['pointer', ['pointer']],
    'fa_pair_snd':                         ['pointer', ['pointer']],    
                                                
    'fa_to_bool':                          ['bool',    ['pointer']],
    'fa_to_int8':                          ['int',     ['pointer']],
    'fa_to_int16':                         ['int',     ['pointer']],
    'fa_peek_int32':                       ['int',     ['pointer']], 
    'fa_peek_int64':                       ['int',     ['pointer']], 
    'fa_peek_float':                       ['double',  ['pointer']], 
    'fa_peek_double':                      ['double',  ['pointer']], 
                                                
    'fa_device_audio_begin_session':       ['pointer', []],
    'fa_device_audio_end_session':         ['void',    ['pointer']],
    'fa_device_audio_with_session':        ['void',    ['pointer','pointer','pointer','pointer']],
    'fa_device_audio_all':                 ['pointer', ['pointer']],
    'fa_device_audio_default':             ['pointer', ['pointer']],
    'fa_device_audio_set_status_callback': ['pointer', ['pointer']],
    'fa_device_audio_name':                ['pointer', ['pointer']],
    'fa_device_audio_host_name':           ['pointer', ['pointer']],
    'fa_device_audio_has_input':           ['bool',    ['pointer']],
    'fa_device_audio_has_output':          ['bool',    ['pointer']],
    'fa_device_audio_input_type':          ['pointer', ['pointer']],
    'fa_device_audio_output_type':         ['pointer', ['pointer']],
    'fa_device_audio_open_stream':         ['pointer', ['pointer','pointer','pointer']],
    'fa_device_audio_close_stream':        ['void',    ['pointer']],
    'fa_device_audio_with_stream':         ['void',    ['pointer','pointer','pointer','pointer','pointer','pointer','pointer']],
    
    'fa_error_check':                      ['bool',    ['pointer']],
    'fa_error_message':                    ['pointer', ['pointer']],
    'fa_string_show':                      ['pointer', ['pointer']],

    'fa_type_channels':                    ['int',     ['pointer']],
})

var bool_ = fa_.fa_to_bool;
var i8_   = fa_.fa_to_int8;
var i16_  = fa_.fa_to_int16;
var i32_  = fa_.fa_peek_int32;
var i64_  = fa_.fa_peek_int64;
var f32_  = fa_.fa_peek_float;
var f64_  = fa_.fa_peek_double;

var pair_ = function (a, importFun) {
    var imp = importFun || dyn_;
    var x   = fa_.fa_pair_fst(a); 
    var y   = fa_.fa_pair_snd(a);
    return [imp(x),imp(y)];
}

var list_   = function(as, importFun) {
    var imp = importFun || dyn_;
    if (fa_.fa_list_is_empty(as)) {
        return [];
    } else {
        var b  = fa_.fa_list_head(as);
        var bs = fa_.fa_list_tail(as);
        return [imp(b)].concat(list_(bs, importFun));
    }
}

var string_     = fa_.fa_string_to_utf8;
var check_      = fa_.fa_error_check;
var message_    = fa_.fa_error_message;
var show_       = fa_.fa_string_show;

var dyn_ = function(a) {
    var t = fa_.fa_dynamic_get_type(a);
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
AudioSession.prototype.end              = function () { return fa.device.audio.endSession(this) }
AudioSession.prototype.devices          = function () { return fa.device.audio.devices(this) }
AudioSession.prototype.defaultDevices   = function () { return fa.device.audio.defaultDevices(this) }

var AudioDevice  = function (a) { this.value = a; showable(this) }
AudioDevice.prototype.name              = function () { return fa.device.audio.name(this) }
AudioDevice.prototype.hostName          = function () { return fa.device.audio.hostName(this) }
AudioDevice.prototype.hasInput          = function () { return fa.device.audio.hasInput(this) }
AudioDevice.prototype.hasOutput         = function () { return fa.device.audio.hasOutput(this) }
AudioDevice.prototype.inputType         = function () { return fa.device.audio.inputType(this) }
AudioDevice.prototype.outputType        = function () { return fa.device.audio.outputType(this) }

var AudioType    = function (a) { this.value = a; showable(this) }
AudioType.prototype.channels            = function () { return fa.type.channels(this) }

var fa = {
    version                     : function() { return dyn_(fa_.fa_audio_engine_version() )},
    versionString               : function() { return string_(fa_.fa_audio_engine_version_string() )} ,
    setLog                      : fa_.fa_audio_engine_set_log,
    setLogStd                   : fa_.fa_audio_engine_set_log_std,
    initialize                  : fa_.fa_audio_engine_initialize,
    terminate                   : fa_.fa_audio_engine_terminate,
    withEngine                  : function(f) { fa.initialize(); f(); fa.terminate(); },

    type : {
        channels                : function(s) { return fa_.fa_type_channels(s.value) },
    },

    device : {
        audio : {
            beginSession        : function () { return o(make(AudioSession), thowIfErr)(fa_.fa_device_audio_begin_session()) },
            endSession          : o(fa_.fa_device_audio_end_session, get("value")),
            withSession         : function(f) { var s = fa.device.audio.beginSession(); f(s); fa.device.audio.endSession(s); },
            devices             : function (s) { return list_(fa_.fa_device_audio_all(s.value), make(AudioDevice))},
            defaultDevices      : function (s) { return pair_(fa_.fa_device_audio_default(s.value), make(AudioDevice))},
            setStatusCallback   : fa_.fa_device_audio_set_status_callback,
            name                : o3(string_,         fa_.fa_device_audio_name,        get("value")),
            hostName            : o3(string_,         fa_.fa_device_audio_host_name,   get("value")),
            hasInput            : o3(id,              fa_.fa_device_audio_has_input,   get("value")),
            hasOutput           : o3(id,              fa_.fa_device_audio_has_output,  get("value")),
            inputType           : o3(make(AudioType), fa_.fa_device_audio_input_type,  get("value")),
            outputType          : o3(make(AudioType), fa_.fa_device_audio_output_type, get("value")),
            openStream          : fa_.fa_device_audio_open_stream,
            closeStream         : fa_.fa_device_audio_close_stream,
            withStream          : fa_.fa_device_audio_with_stream,
            Session             : AudioSession,
            Device              : AudioDevice,
            Type                : AudioType,
        }
    },
    
}

module.exports = fa;
