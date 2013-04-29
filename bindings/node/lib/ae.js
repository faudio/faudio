
var ffi      = require('ffi');
var cliColor = require('cli-color');

var ae_ = ffi.Library('libae', {
    'doremir_audio_engine_version':             ['pointer', [] ],
    'doremir_audio_engine_version_string':      ['pointer', [] ],
    'doremir_string_to_utf8':                   ['string',  ['pointer'] ],
                                                
    'doremir_audio_engine_initialize':          ['void',    [] ],
    'doremir_audio_engine_terminate':           ['void',    [] ],
                                                
    'doremir_audio_engine_set_log':             ['void',    ['pointer','pointer']],
    'doremir_audio_engine_set_log_std':         ['void',    []],
                                                
                                                
    'doremir_list_empty':                       ['pointer', []],
    'doremir_list_cons':                        ['pointer', ['pointer','pointer']],
    'doremir_list_is_empty':                    ['bool',    ['pointer']],
    'doremir_list_head':                        ['pointer', ['pointer']],
    'doremir_list_tail':                        ['pointer', ['pointer']],
                                                
    'doremir_dynamic_get_type':                 ['int',     ['pointer']],
                                                
    'doremir_pair_create':                      ['pointer', ['pointer', 'pointer']],
    'doremir_pair_fst':                         ['pointer', ['pointer']],
    'doremir_pair_snd':                         ['pointer', ['pointer']],    
                                                
    'doremir_to_bool':                          ['bool',    ['pointer']],
    'doremir_to_int8':                          ['int',     ['pointer']],
    'doremir_to_int16':                         ['int',     ['pointer']],
    'doremir_peek_int32':                       ['int',     ['pointer']], 
    'doremir_peek_int64':                       ['int',     ['pointer']], 
    'doremir_peek_float':                       ['double',  ['pointer']], 
    'doremir_peek_double':                      ['double',  ['pointer']], 
                                                
    'doremir_device_audio_begin_session':       ['pointer', []],
    'doremir_device_audio_end_session':         ['void',    ['pointer']],
    'doremir_device_audio_with_session':        ['void',    ['pointer','pointer','pointer','pointer']],
    'doremir_device_audio_all':                 ['pointer', ['pointer']],
    'doremir_device_audio_default':             ['pointer', ['pointer']],
    'doremir_device_audio_set_status_callback': ['pointer', ['pointer']],
    'doremir_device_audio_name':                ['pointer', ['pointer']],
    'doremir_device_audio_host_name':           ['pointer', ['pointer']],
    'doremir_device_audio_has_input':           ['bool',    ['pointer']],
    'doremir_device_audio_has_output':          ['bool',    ['pointer']],
    'doremir_device_audio_input_type':          ['pointer', ['pointer']],
    'doremir_device_audio_output_type':         ['pointer', ['pointer']],
    'doremir_device_audio_open_stream':         ['pointer', ['pointer','pointer','pointer']],
    'doremir_device_audio_close_stream':        ['void',    ['pointer']],
    'doremir_device_audio_with_stream':         ['void',    ['pointer','pointer','pointer','pointer','pointer','pointer','pointer']],
    
    'doremir_error_check':                      ['bool', ['pointer']],
    'doremir_error_message':                    ['pointer', ['pointer']],
    'doremir_string_show':                      ['pointer', ['pointer']],
})

var bool_ = ae_.doremir_to_bool;
var i8_   = ae_.doremir_to_int8;
var i16_  = ae_.doremir_to_int16;
var i32_  = ae_.doremir_peek_int32;
var i64_  = ae_.doremir_peek_int64;
var f32_  = ae_.doremir_peek_float;
var f64_  = ae_.doremir_peek_double;

var pair_ = function (a) {
    var x = ae_.doremir_pair_fst(as); 
    var y = ae_.doremir_pair_snd(as);
    return [x,y];
}

var list_   = function(as, importFun) {
    var imp = importFun || dyn_;
    if (ae_.doremir_list_is_empty(as)) {
        return [];
    } else {
        var b  = ae_.doremir_list_head(as);
        var bs = ae_.doremir_list_tail(as);
        return [imp(b)].concat(list_(bs, importFun));
    }
}

var string_     = ae_.doremir_string_to_utf8;
var check_      = ae_.doremir_error_check;
var message_    = ae_.doremir_error_message;
var show_       = ae_.doremir_string_show;

var dyn_ = function(a) {
    var t = ae_.doremir_dynamic_get_type(a);
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
var AudioDevice  = function (a) { this.value = a; showable(this) }


var make = function(c) {
    return function (a) { return new c(a) }
}
var id = function(a) { return a };


var ae = {
    version                     : function() { return dyn_(
                                    ae_.doremir_audio_engine_version() )},
    versionString               : function() { return string_(
                                    ae_.doremir_audio_engine_version_string() )} ,
    initialize                  : ae_.doremir_audio_engine_initialize,
    terminate                   : ae_.doremir_audio_engine_terminate,
    withEngine                  : function(f) {
        ae.initialize();
        f();
        ae.terminate();
    },
    
    setLog                      : ae_.doremir_audio_engine_set_log,
    setLogStd                   : ae_.doremir_audio_engine_set_log_std,

    device : {
        audio : {
            beginSession        : function () { return make(AudioSession)((thowIfErr(
                                    ae_.doremir_device_audio_begin_session() )))},
            endSession          : function(s) { 
                                    ae_.doremir_device_audio_end_session(s.value) },
            withSession         : function(f) {
                                    var s = ae.device.audio.beginSession();
                                    f(s);
                                    ae.device.audio.endSession(s);
                                },


            all                 : function (s) { return list_(
                                    ae_.doremir_device_audio_all(s.value), make(AudioDevice))},
            defaults            : function (s) { return list_(
                                    ae_.doremir_device_audio_default() )},
            setStatusCallback   : ae_.doremir_device_audio_set_status_callback,

            name                : function(s) { return string_(ae_.doremir_device_audio_name(s.value)) },
            hostName            : function(s) { return string_(ae_.doremir_device_audio_host_name(s.value)) },
            hasInput            : function(s) { return ae_.doremir_device_audio_has_input(s.value) },
            hasOutput           : function(s) { return ae_.doremir_device_audio_has_output(s.value) },

            inputType           : ae_.doremir_device_audio_input_type,
            outputType          : ae_.doremir_device_audio_output_type,
            openStream          : ae_.doremir_device_audio_open_stream,
            closeStream         : ae_.doremir_device_audio_close_stream,
            withStream          : ae_.doremir_device_audio_with_stream,
        }
    },

    string : {
        toUtf8      : ae_.doremir_string_to_utf8,
    },
    
    AudioSession : AudioSession,
    AudioDevice  : AudioDevice,
}

AudioSession.prototype.end = function () { return ae.device.audio.endSession(this) }
AudioSession.prototype.devices = function () { return ae.device.audio.all(this) }
AudioDevice.prototype.name = function () { return ae.device.audio.name(this) }
AudioDevice.prototype.hostName = function () { return ae.device.audio.hostName(this) }
AudioDevice.prototype.hasInput = function () { return ae.device.audio.hasInput(this) }
AudioDevice.prototype.hasOutput = function () { return ae.device.audio.hasOutput(this) }
AudioDevice.prototype.inputType = function () { return ae.device.audio.inputType(this) }
AudioDevice.prototype.outputType = function () { return ae.device.audio.outputType(this) }

module.exports = ae;
