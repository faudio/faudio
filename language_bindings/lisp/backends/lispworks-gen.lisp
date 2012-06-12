#| DATE           : 13 Mar 2012 
 | USER           : hans 
 | PROCESSED FILE : /Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio.h
 |#

(in-package :audio-engine)

;;; Derived from file : "/usr/lib/gcc/i686-apple-darwin10/4.2.1/include/stdint.h"

(fli:define-c-typedef (int8-t (:foreign-name "int8_t")) (:signed :char))
(fli:define-c-typedef (int16-t (:foreign-name "int16_t")) :short)
(fli:define-c-typedef (int32-t (:foreign-name "int32_t")) :int)
(fli:define-c-typedef (int64-t (:foreign-name "int64_t")) :long-long)
(fli:define-c-typedef (uint8-t (:foreign-name "uint8_t"))
                      (:unsigned :char))
(fli:define-c-typedef (uint16-t (:foreign-name "uint16_t"))
                      (:unsigned :short))
(fli:define-c-typedef (uint32-t (:foreign-name "uint32_t"))
                      (:unsigned :int))
(fli:define-c-typedef (uint64-t (:foreign-name "uint64_t"))
                      (:unsigned :long-long))
(fli:define-c-typedef (int-least8-t (:foreign-name "int_least8_t"))
                      int8-t)
(fli:define-c-typedef (int-least16-t (:foreign-name "int_least16_t"))
                      int16-t)
(fli:define-c-typedef (int-least32-t (:foreign-name "int_least32_t"))
                      int32-t)
(fli:define-c-typedef (int-least64-t (:foreign-name "int_least64_t"))
                      int64-t)
(fli:define-c-typedef (uint-least8-t (:foreign-name "uint_least8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-least16-t (:foreign-name "uint_least16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-least32-t (:foreign-name "uint_least32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-least64-t (:foreign-name "uint_least64_t"))
                      uint64-t)
(fli:define-c-typedef (int-fast8-t (:foreign-name "int_fast8_t"))
                      int8-t)
(fli:define-c-typedef (int-fast16-t (:foreign-name "int_fast16_t"))
                      int16-t)
(fli:define-c-typedef (int-fast32-t (:foreign-name "int_fast32_t"))
                      int32-t)
(fli:define-c-typedef (int-fast64-t (:foreign-name "int_fast64_t"))
                      int64-t)
(fli:define-c-typedef (uint-fast8-t (:foreign-name "uint_fast8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-fast16-t (:foreign-name "uint_fast16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-fast32-t (:foreign-name "uint_fast32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-fast64-t (:foreign-name "uint_fast64_t"))
                      uint64-t)
(fli:define-c-typedef (intptr-t (:foreign-name "intptr_t")) :long)
(fli:define-c-typedef (uintptr-t (:foreign-name "uintptr_t"))
                      (:unsigned :long))
(fli:define-c-typedef (intmax-t (:foreign-name "intmax_t")) :long)
(fli:define-c-typedef (uintmax-t (:foreign-name "uintmax_t"))
                      (:unsigned :long))

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/numeric.h"

(fli:define-c-typedef (sclint16 (:foreign-name "SclInt16")) int16-t)
(fli:define-c-typedef (sclint32 (:foreign-name "SclInt32")) int32-t)
(fli:define-c-typedef (sclint64 (:foreign-name "SclInt64")) int64-t)
(fli:define-c-typedef (sclword16 (:foreign-name "SclWord16")) uint16-t)
(fli:define-c-typedef (sclword32 (:foreign-name "SclWord32")) uint32-t)
(fli:define-c-typedef (sclword64 (:foreign-name "SclWord64")) uint64-t)
(fli:define-c-typedef (sclchar (:foreign-name "SclChar"))
                      (:unsigned :short))
(fli:define-c-typedef (sclfloat32 (:foreign-name "SclFloat32")) :float)
(fli:define-c-typedef (sclfloat64 (:foreign-name "SclFloat64")) :double)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/enum.h"

(fli:define-c-typedef (sclatomtype (:foreign-name "SclAtomType")) :int)
(fli:define-c-typedef (sclmessagekind (:foreign-name "SclMessageKind"))
                      :int)
(fli:define-c-typedef (scltimeunit (:foreign-name "SclTimeUnit")) :int)
(fli:define-c-typedef (sclinterruptionmode
                       (:foreign-name "SclInterruptionMode"))
                      :int)
(fli:define-c-typedef (sclstreamtype (:foreign-name "SclStreamType"))
                      :int)
(fli:define-foreign-function (scl-message-type-audio "scl_message_type_audio"
                                                     :source)
                             nil
                             :result-type
                             sclmessagekind
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-message-type-midi "scl_message_type_midi"
                                                    :source)
                             nil
                             :result-type
                             sclmessagekind
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-time-unit-samples "scl_time_unit_samples"
                                                    :source)
                             nil
                             :result-type
                             scltimeunit
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-time-unit-milliseconds "scl_time_unit_milliseconds"
                                                         :source)
                             nil
                             :result-type
                             scltimeunit
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-string "scl_atom_string"
                                              :source)
                             nil
                             :result-type
                             sclatomtype
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-int "scl_atom_int" :source)
                             nil
                             :result-type
                             sclatomtype
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-double "scl_atom_double"
                                              :source)
                             nil
                             :result-type
                             sclatomtype
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-interruption-mode-simple "scl_interruption_mode_simple"
                                                           :source)
                             nil
                             :result-type
                             sclinterruptionmode
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-interruption-mode-forcing "scl_interruption_mode_forcing"
                                                            :source)
                             nil
                             :result-type
                             sclinterruptionmode
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-interruption-mode-transactional "scl_interruption_mode_transactional"
                                                                  :source)
                             nil
                             :result-type
                             sclinterruptionmode
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/string.h"

(fli:define-c-typedef (sclstring (:foreign-name "SclString"))
                      (:pointer sclchar))
(fli:define-foreign-function (scl-schars-to-string "scl_schars_to_string"
                                                   :source)
                             ((val (:pointer (:signed :char))))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-uchars-to-string "scl_uchars_to_string"
                                                   :source)
                             ((val (:pointer (:unsigned :char))))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-string-to-schars "scl_string_to_schars"
                                                   :source)
                             ((val sclstring))
                             :result-type
                             (:pointer (:unsigned :char))
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-string-to-uchars "scl_string_to_uchars"
                                                   :source)
                             ((val sclstring))
                             :result-type
                             (:pointer (:signed :char))
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/atom.h"

(fli:define-c-typedef (sclatom (:foreign-name "SclAtom"))
                      (:pointer :void))
(fli:define-foreign-function (scl-atom-type "scl_atom_type" :source)
                             ((atom sclatom))
                             :result-type
                             sclatomtype
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-to-int "scl_atom_to_int"
                                              :source)
                             ((atom sclatom))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-to-double "scl_atom_to_double"
                                                 :source)
                             ((atom sclatom))
                             :result-type
                             :double
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-to-string "scl_atom_to_string"
                                                 :source)
                             ((atom sclatom))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-from-int "scl_atom_from_int"
                                                :source)
                             ((value :int))
                             :result-type
                             sclatom
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-from-double "scl_atom_from_double"
                                                   :source)
                             ((value :double))
                             :result-type
                             sclatom
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-atom-from-string "scl_atom_from_string"
                                                   :source)
                             ((value sclstring))
                             :result-type
                             sclatom
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/time.h"

(fli:define-c-typedef (sclrealtime (:foreign-name "SclRealTime"))
                      :double)
(fli:define-c-typedef (scltime (:foreign-name "SclTime")) :int)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/error.h"

(fli:define-c-typedef (sclerror (:foreign-name "SclError"))
                      (:pointer :void))
(fli:define-c-typedef (sclportaudioerror
                       (:foreign-name "SclPortaudioError"))
                      (:pointer :void))
(fli:define-c-typedef (sclportmidierror
                       (:foreign-name "SclPortmidiError"))
                      (:pointer :void))
(fli:define-c-typedef (scldsperror (:foreign-name "SclDspError"))
                      (:pointer :void))
(fli:define-c-typedef (sclstreamerror (:foreign-name "SclStreamError"))
                      (:pointer :void))
(fli:define-c-typedef (sclerrorhandler
                       (:foreign-name "SclErrorHandler"))
                      (:pointer (:function (scltime sclerror) :void)))
(fli:define-foreign-function (scl-error-message "scl_error_message"
                                                :source)
                             ((obj sclerror))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-portaudio-error-code "scl_portaudio_error_code"
                                                       :source)
                             ((obj sclportaudioerror))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-portmidi-error-code "scl_portmidi_error_code"
                                                      :source)
                             ((obj sclportmidierror))
                             :result-type
                             :int
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/device/audio.h"

(fli:define-c-typedef (sclaudiohost (:foreign-name "SclAudioHost"))
                      (:pointer :void))
(fli:define-c-typedef (sclaudiodevice (:foreign-name "SclAudioDevice"))
                      (:pointer :void))
(fli:define-foreign-function (scl-audio-host-name "scl_audio_host_name"
                                                  :source)
                             ((obj sclaudiohost))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-host-number-of-devices "scl_audio_host_number_of_devices"
                                                               :source)
                             ((obj sclaudiohost))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-host-devices "scl_audio_host_devices"
                                                     :source)
                             ((obj sclaudiohost) (len (:pointer :int)))
                             :result-type
                             (:pointer sclaudiodevice)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-hosts "scl_audio_hosts"
                                              :source)
                             ((len (:pointer :int))
                              (err (:pointer sclportaudioerror)))
                             :result-type
                             (:pointer sclaudiohost)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-audio-host "scl_default_audio_host"
                                                     :source)
                             ((err (:pointer sclportaudioerror)))
                             :result-type
                             sclaudiohost
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-name "scl_audio_device_name"
                                                    :source)
                             ((obj sclaudiodevice))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-host "scl_audio_device_host"
                                                    :source)
                             ((obj sclaudiodevice))
                             :result-type
                             sclaudiohost
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-max-input-channels "scl_audio_device_max_input_channels"
                                                                  :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-max-output-channels "scl_audio_device_max_output_channels"
                                                                   :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-default-low-input-latency "scl_audio_device_default_low_input_latency"
                                                                         :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-default-high-input-latency "scl_audio_device_default_high_input_latency"
                                                                          :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-default-low-output-latency "scl_audio_device_default_low_output_latency"
                                                                          :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-default-high-output-latency "scl_audio_device_default_high_output_latency"
                                                                           :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-audio-device-default-sample-rate "scl_audio_device_default_sample_rate"
                                                                   :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-audio-input-device "scl_default_audio_input_device"
                                                             :source)
                             ((err (:pointer sclportaudioerror)))
                             :result-type
                             sclaudiodevice
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-audio-output-device "scl_default_audio_output_device"
                                                              :source)
                             ((err (:pointer sclportaudioerror)))
                             :result-type
                             sclaudiodevice
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/device/midi.h"

(fli:define-c-typedef (sclmididevice (:foreign-name "SclMidiDevice"))
                      (:pointer :void))
(fli:define-foreign-function (scl-midi-device-name "scl_midi_device_name"
                                                   :source)
                             ((obj sclmididevice))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-midi-device-host-name "scl_midi_device_host_name"
                                                        :source)
                             ((obj sclmididevice))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-midi-device-has-input "scl_midi_device_has_input"
                                                        :source)
                             ((obj sclmididevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-midi-device-has-output "scl_midi_device_has_output"
                                                         :source)
                             ((obj sclmididevice))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-midi-input-device "scl_default_midi_input_device"
                                                            :source)
                             ((err (:pointer sclportmidierror)))
                             :result-type
                             sclmididevice
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-midi-output-device "scl_default_midi_output_device"
                                                             :source)
                             ((err (:pointer sclportmidierror)))
                             :result-type
                             sclmididevice
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-midi-devices "scl_midi_devices"
                                               :source)
                             ((length (:pointer :int))
                              (err (:pointer sclportmidierror)))
                             :result-type
                             (:pointer sclmididevice)
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/processor.h"

(fli:define-c-typedef (sclaudioprocessor
                       (:foreign-name "SclAudioProcessor"))
                      (:pointer :void))
(fli:define-foreign-function (scl-processor-controls "scl_processor_controls"
                                                     :source)
                             ((obj sclaudioprocessor)
                              (length (:pointer :int)))
                             :result-type
                             (:pointer sclstring)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-processor-num-inputs "scl_processor_num_inputs"
                                                       :source)
                             ((obj sclaudioprocessor))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-processor-num-outputs "scl_processor_num_outputs"
                                                        :source)
                             ((obj sclaudioprocessor))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-sequence "scl_sequence" :source)
                             ((objs (:pointer sclaudioprocessor))
                              (len :int)
                              (err (:pointer scldsperror)))
                             :result-type
                             sclaudioprocessor
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-parallel "scl_parallel" :source)
                             ((objs (:pointer sclaudioprocessor))
                              (len :int)
                              (err (:pointer scldsperror)))
                             :result-type
                             sclaudioprocessor
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-load-fluidsynth "scl_load_fluidsynth"
                                                  :source)
                             ((path sclstring)
                              (err (:pointer scldsperror)))
                             :result-type
                             sclaudioprocessor
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/stream.h"

(fli:define-c-typedef (sclstream (:foreign-name "SclStream"))
                      (:pointer :void))
(fli:define-foreign-function (scl-stream-sample-rate "scl_stream_sample_rate"
                                                     :source)
                             ((stream sclstream))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-stream-audio-buffer-size "scl_stream_audio_buffer_size"
                                                           :source)
                             ((stream sclstream))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-stream-running "scl_stream_running"
                                                 :source)
                             ((stream sclstream))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-stream-start "scl_stream_start"
                                               :source)
                             ((stream sclstream)
                              (err1 (:pointer sclportaudioerror))
                              (err2 (:pointer sclportmidierror))
                              (err3 (:pointer scldsperror)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-stream-stop "scl_stream_stop"
                                              :source)
                             ((stream sclstream)
                              (err1 (:pointer sclportaudioerror))
                              (err2 (:pointer sclportmidierror))
                              (err3 (:pointer scldsperror)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-stream-abort "scl_stream_abort"
                                               :source)
                             ((stream sclstream)
                              (err1 (:pointer sclportaudioerror))
                              (err2 (:pointer sclportmidierror))
                              (err3 (:pointer scldsperror)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-stream-set-error-handler "scl_stream_set_error_handler"
                                                           :source)
                             ((stream sclstream)
                              (handler sclerrorhandler))
                             :result-type
                             :void
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/stream/device.h"

(fli:define-c-typedef (scldevicestreamoptions
                       (:foreign-name "SclDeviceStreamOptions"))
                      (:pointer :void))
(fli:define-foreign-function (scl-open-device-stream "scl_open_device_stream"
                                                     :source)
                             ((midi-in sclmididevice)
                              (midi-out sclmididevice)
                              (audio-in sclaudiodevice)
                              (audio-out sclaudiodevice)
                              (processor sclaudioprocessor)
                              (options scldevicestreamoptions)
                              (pm-err (:pointer sclportmidierror))
                              (pa-err (:pointer sclportaudioerror))
                              (dsp-err (:pointer scldsperror)))
                             :result-type
                             sclstream
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-device-stream-options "scl_default_device_stream_options"
                                                                :source)
                             nil
                             :result-type
                             scldevicestreamoptions
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-get-sample-rate "scl_device_stream_options_get_sample_rate"
                                                                        :source)
                             ((obj scldevicestreamoptions))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-get-audio-buffer-size "scl_device_stream_options_get_audio_buffer_size"
                                                                              :source)
                             ((obj scldevicestreamoptions))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-get-audio-latency "scl_device_stream_options_get_audio_latency"
                                                                          :source)
                             ((obj scldevicestreamoptions))
                             :result-type
                             sclrealtime
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-get-midi-latency "scl_device_stream_options_get_midi_latency"
                                                                         :source)
                             ((obj scldevicestreamoptions))
                             :result-type
                             sclrealtime
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-is-non-blocking
                              "scl_device_stream_options_is_non_blocking"
                              :source)
                             ((obj scldevicestreamoptions))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-is-exclusive-mode
                              "scl_device_stream_options_is_exclusive_mode"
                              :source)
                             ((obj scldevicestreamoptions))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-set-sample-rate "scl_device_stream_options_set_sample_rate"
                                                                        :source)
                             ((obj scldevicestreamoptions)
                              (value :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-set-audio-buffer-size "scl_device_stream_options_set_audio_buffer_size"
                                                                              :source)
                             ((obj scldevicestreamoptions)
                              (value :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-set-audio-latency "scl_device_stream_options_set_audio_latency"
                                                                          :source)
                             ((obj scldevicestreamoptions)
                              (value sclrealtime))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-set-midi-latency "scl_device_stream_options_set_midi_latency"
                                                                         :source)
                             ((obj scldevicestreamoptions)
                              (value sclrealtime))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-set-non-blocking
                              "scl_device_stream_options_set_non_blocking"
                              :source)
                             ((obj scldevicestreamoptions)
                              (value :int))
                             :result-type
                             sclrealtime
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-device-stream-options-set-exclusive-mode
                              "scl_device_stream_options_set_exclusive_mode"
                              :source)
                             ((obj scldevicestreamoptions)
                              (value :int))
                             :result-type
                             sclrealtime
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/scheduling/future.h"

(fli:define-c-typedef (sclinterruptable
                       (:foreign-name "SclInterruptable"))
                      (:pointer :void))
(fli:define-c-typedef (sclfuture (:foreign-name "SclFuture"))
                      (:pointer :void))
(fli:define-c-typedef (sclfuturegroup (:foreign-name "SclFutureGroup"))
                      (:pointer :void))
(fli:define-foreign-function (scl-interrupt-future "scl_interrupt_future"
                                                   :source)
                             ((obj sclfuture))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-interrupt-future-group "scl_interrupt_future_group"
                                                         :source)
                             ((obj sclfuturegroup))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-new-future-group "scl_new_future_group"
                                                   :source)
                             ((mode sclinterruptionmode))
                             :result-type
                             sclfuturegroup
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-future-group-interruption-mode "scl_future_group_interruption_mode"
                                                                 :source)
                             ((group sclfuturegroup))
                             :result-type
                             sclinterruptionmode
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/scheduling/action.h"

(fli:define-c-typedef (sclscheduleoptions
                       (:foreign-name "SclScheduleOptions"))
                      (:pointer :void))
(fli:define-c-typedef (sclaction (:foreign-name "SclAction"))
                      (:pointer (:function (scltime) :void)))
(fli:define-foreign-function (scl-default-schedule-options "scl_default_schedule_options"
                                                           :source)
                             nil
                             :result-type
                             sclscheduleoptions
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-get-unit "scl_schedule_options_get_unit"
                                                            :source)
                             ((obj sclscheduleoptions))
                             :result-type
                             scltimeunit
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-get-groups "scl_schedule_options_get_groups"
                                                              :source)
                             ((obj sclscheduleoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer sclfuturegroup)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-get-repeats "scl_schedule_options_get_repeats"
                                                               :source)
                             ((obj sclscheduleoptions))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-get-interval "scl_schedule_options_get_interval"
                                                                :source)
                             ((obj sclscheduleoptions))
                             :result-type
                             scltime
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-set-unit "scl_schedule_options_set_unit"
                                                            :source)
                             ((obj sclscheduleoptions)
                              (unit scltimeunit))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-set-groups "scl_schedule_options_set_groups"
                                                              :source)
                             ((obj sclscheduleoptions)
                              (groups (:pointer sclfuturegroup))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-set-repeats "scl_schedule_options_set_repeats"
                                                               :source)
                             ((obj sclscheduleoptions) (repeats :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-options-set-interval "scl_schedule_options_set_interval"
                                                                :source)
                             ((obj sclscheduleoptions)
                              (interval scltime))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-now "scl_schedule_now"
                                               :source)
                             ((stream sclstream)
                              (action sclaction)
                              (opts sclscheduleoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-later "scl_schedule_later"
                                                 :source)
                             ((stream sclstream)
                              (time scltime)
                              (action sclaction)
                              (opts sclscheduleoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-schedule-at "scl_schedule_at"
                                              :source)
                             ((stream sclstream)
                              (time scltime)
                              (action sclaction)
                              (opts sclscheduleoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/scheduling/message.h"

(fli:define-c-typedef (sclsendoptions (:foreign-name "SclSendOptions"))
                      (:pointer :void))
(fli:define-c-typedef (sclreceiveoptions
                       (:foreign-name "SclReceiveOptions"))
                      (:pointer :void))
(fli:define-c-typedef (sclmessageinfo (:foreign-name "SclMessageInfo"))
                      (:pointer :void))
(fli:define-c-typedef (sclreceiver (:foreign-name "SclReceiver"))
                      (:pointer
                       (:function
                        (scltime (:pointer sclatom) :int)
                        :void)))
(fli:define-c-typedef (sclbufferedreceiver
                       (:foreign-name "SclBufferedReceiver"))
                      (:pointer
                       (:function
                        (scltime
                         (:pointer (:pointer sclatom))
                         (:pointer :int)
                         :int)
                        :void)))
(fli:define-foreign-function (scl-default-send-options "scl_default_send_options"
                                                       :source)
                             nil
                             :result-type
                             sclscheduleoptions
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-get-kind "scl_send_options_get_kind"
                                                        :source)
                             ((obj sclsendoptions))
                             :result-type
                             sclmessagekind
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-get-processors "scl_send_options_get_processors"
                                                              :source)
                             ((obj sclsendoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer sclaudioprocessor)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-get-devices "scl_send_options_get_devices"
                                                           :source)
                             ((obj sclsendoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer sclmididevice)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-get-channels "scl_send_options_get_channels"
                                                            :source)
                             ((obj sclsendoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer :int)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-set-kind "scl_send_options_set_kind"
                                                        :source)
                             ((obj sclsendoptions)
                              (kind sclmessagekind))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-set-processors "scl_send_options_set_processors"
                                                              :source)
                             ((obj sclsendoptions)
                              (procs (:pointer sclaudioprocessor))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-set-devices "scl_send_options_set_devices"
                                                           :source)
                             ((obj sclsendoptions)
                              (devices (:pointer sclmididevice))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-options-set-channels "scl_send_options_set_channels"
                                                            :source)
                             ((obj sclsendoptions)
                              (channels (:pointer :int))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-default-receive-options "scl_default_receive_options"
                                                          :source)
                             nil
                             :result-type
                             sclscheduleoptions
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-get-kind "scl_receive_options_get_kind"
                                                           :source)
                             ((obj sclreceiveoptions))
                             :result-type
                             sclmessagekind
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-get-processors "scl_receive_options_get_processors"
                                                                 :source)
                             ((obj sclreceiveoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer sclaudioprocessor)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-get-devices "scl_receive_options_get_devices"
                                                              :source)
                             ((obj sclreceiveoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer sclmididevice)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-get-channels "scl_receive_options_get_channels"
                                                               :source)
                             ((obj sclreceiveoptions)
                              (len (:pointer :int)))
                             :result-type
                             (:pointer :int)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-set-kind "scl_receive_options_set_kind"
                                                           :source)
                             ((obj sclreceiveoptions)
                              (kind sclmessagekind))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-set-processors "scl_receive_options_set_processors"
                                                                 :source)
                             ((obj sclreceiveoptions)
                              (procs (:pointer sclaudioprocessor))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-set-devices "scl_receive_options_set_devices"
                                                              :source)
                             ((obj sclreceiveoptions)
                              (devices (:pointer sclmididevice))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-options-set-channels "scl_receive_options_set_channels"
                                                               :source)
                             ((obj sclreceiveoptions)
                              (channels (:pointer :int))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-now "scl_send_now" :source)
                             ((stream sclstream)
                              (message (:pointer sclatom))
                              (len :int)
                              (opts sclsendoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-later "scl_send_later" :source)
                             ((stream sclstream)
                              (time scltime)
                              (message (:pointer sclatom))
                              (len :int)
                              (opts sclsendoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-send-at "scl_send_at" :source)
                             ((stream sclstream)
                              (time scltime)
                              (message (:pointer sclatom))
                              (len :int)
                              (opts sclsendoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive "scl_receive" :source)
                             ((stream sclstream)
                              (receiver sclreceiver)
                              (opts sclreceiveoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-receive-buffered "scl_receive_buffered"
                                                   :source)
                             ((stream sclstream)
                              (receiver sclbufferedreceiver)
                              (opts sclreceiveoptions)
                              (err (:pointer sclstreamerror)))
                             :result-type
                             sclfuture
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/test.h"

(fli:define-foreign-function (scl-test-nothing "scl_test_nothing"
                                               :source)
                             nil
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-error "scl_test_error" :source)
                             ((variant :int) (err (:pointer sclerror)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-two-errors "scl_test_two_errors"
                                                  :source)
                             ((variant :int)
                              (merr (:pointer sclportmidierror))
                              (aerr (:pointer sclportaudioerror)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-int "scl_test_pass_int"
                                                :source)
                             ((x :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-float "scl_test_pass_float"
                                                  :source)
                             ((x :float))
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-double "scl_test_pass_double"
                                                   :source)
                             ((x :double))
                             :result-type
                             :double
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-enum "scl_test_pass_enum"
                                                 :source)
                             ((x scltimeunit))
                             :result-type
                             scltimeunit
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-string "scl_test_pass_string"
                                                   :source)
                             ((x sclstring))
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-atom "scl_test_pass_atom"
                                                 :source)
                             ((x sclatom))
                             :result-type
                             sclatom
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-object "scl_test_pass_object"
                                                   :source)
                             ((obj sclaudiodevice))
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-list-int "scl_test_pass_list_int"
                                                     :source)
                             ((x (:pointer :int)) (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-list-string "scl_test_pass_list_string"
                                                        :source)
                             ((x (:pointer sclstring)) (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-list-object "scl_test_pass_list_object"
                                                        :source)
                             ((x (:pointer (:pointer :void)))
                              (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-list-atom "scl_test_pass_list_atom"
                                                      :source)
                             ((x (:pointer sclatom)) (len :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-pass-list-list-int "scl_test_pass_list_list_int"
                                                          :source)
                             ((x (:pointer (:pointer :int)))
                              (len1 (:pointer :int))
                              (len2 :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-int "scl_test_return_int"
                                                  :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-float "scl_test_return_float"
                                                    :source)
                             nil
                             :result-type
                             :float
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-double "scl_test_return_double"
                                                     :source)
                             nil
                             :result-type
                             :double
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-enum "scl_test_return_enum"
                                                   :source)
                             nil
                             :result-type
                             scltimeunit
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-string "scl_test_return_string"
                                                     :source)
                             nil
                             :result-type
                             sclstring
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-atom "scl_test_return_atom"
                                                   :source)
                             nil
                             :result-type
                             sclatom
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-object "scl_test_return_object"
                                                     :source)
                             nil
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-list-int "scl_test_return_list_int"
                                                       :source)
                             ((len (:pointer :int)))
                             :result-type
                             (:pointer :int)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-list-string "scl_test_return_list_string"
                                                          :source)
                             ((len (:pointer :int)))
                             :result-type
                             (:pointer sclstring)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-list-object "scl_test_return_list_object"
                                                          :source)
                             ((len (:pointer :int)))
                             :result-type
                             (:pointer (:pointer :void))
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-list-atom "scl_test_return_list_atom"
                                                        :source)
                             ((len (:pointer :int)))
                             :result-type
                             (:pointer sclatom)
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-test-return-list-list-int "scl_test_return_list_list_int"
                                                            :source)
                             ((len1 (:pointer (:pointer :int)))
                              (len2 (:pointer :int)))
                             :result-type
                             (:pointer (:pointer :int))
                             :language
                             :ansi-c)

;;; Derived from file : "/Users/hans/Documents/Kod/doremir/modus/audio/include/sclaudio/memory.h"

(fli:define-foreign-function (scl-free-error "scl_free_error" :source)
                             ((obj sclerror))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-portaudio-error "scl_free_portaudio_error"
                                                       :source)
                             ((obj sclportaudioerror))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-portmidi-error "scl_free_portmidi_error"
                                                      :source)
                             ((obj sclportmidierror))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-audio-host "scl_free_audio_host"
                                                  :source)
                             ((obj sclaudiohost))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-audio-device "scl_free_audio_device"
                                                    :source)
                             ((obj sclaudiodevice))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-midi-device "scl_free_midi_device"
                                                   :source)
                             ((obj sclmididevice))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-future "scl_free_future"
                                              :source)
                             ((obj sclfuture))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-stream "scl_free_stream"
                                              :source)
                             ((obj sclstream))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-processor "scl_free_processor"
                                                 :source)
                             ((obj sclaudioprocessor))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-future-group "scl_free_future_group"
                                                    :source)
                             ((obj sclfuturegroup))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-atom "scl_free_atom" :source)
                             ((obj sclatom))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-schedule-options "scl_free_schedule_options"
                                                        :source)
                             ((opts sclscheduleoptions))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-send-options "scl_free_send_options"
                                                    :source)
                             ((opts sclsendoptions))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-receive-options "scl_free_receive_options"
                                                       :source)
                             ((opts sclreceiveoptions))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scl-free-array "scl_free_array" :source)
                             ((arr (:pointer (:pointer :void))))
                             :result-type
                             :void
                             :language
                             :ansi-c)