

Sources, sinks and activators
========

The audio engine use a simple categorization of devices called *sources*, *sinks* and *activators*. Sources and sinks produce and consume data respectively, while activators provide notifications data data is to be produced or consumed.

Stream composition refers to the process of composing 




    DeviceInOutStream
    DeviceInStream
    DeviceOutStream
    FileInStream
    FileOutStream
    BufferInStream
    BufferOutStream

    define-processor x y
        (+ x y)
        
