
all:	c
bindings: lisp

c:
	mkdir -p include/fae
	mkdir -p include/fae/atomic
	mkdir -p include/fae/device
	mkdir -p include/fae/processor
	mkdir -p include/fae/processor/external
	mkdir -p include/fae/string
	mkdir -p include/fae/system
	mkdir -p include/fae/thread
	modulo -Mmodules --language=c    <modules/Fae.module    			>include/fae.h && \
	modulo -Mmodules --language=c    <modules/Fae/AudioEngine.module    	>include/fae/audio_engine.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic.module    		>include/fae/atomic.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic/Queue.module       	>include/fae/atomic/queue.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic/Stack.module       	>include/fae/atomic/stack.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic/RingBuffer.module  	>include/fae/atomic/ring_buffer.h && \
	modulo -Mmodules --language=c    <modules/Fae/System.module    		>include/fae/system.h && \
	modulo -Mmodules --language=c    <modules/Fae/System/Directory.module    	>include/fae/system/directory.h && \
	modulo -Mmodules --language=c    <modules/Fae/System/Event.module    	>include/fae/system/event.h && \
	modulo -Mmodules --language=c    <modules/Fae/Dynamic.module    		>include/fae/dynamic.h && \
	modulo -Mmodules --language=c    <modules/Fae/Event.module    		>include/fae/event.h && \
	modulo -Mmodules --language=c    <modules/Fae/Thread.module    		>include/fae/thread.h && \
	modulo -Mmodules --language=c    <modules/Fae/Std.module    		>include/fae/std.h && \
	modulo -Mmodules --language=c    <modules/Fae/String.module    		>include/fae/string.h && \
	modulo -Mmodules --language=c    <modules/Fae/String/List.module    	>include/fae/string/list.h && \
	modulo -Mmodules --language=c    <modules/Fae/Error.module    		>include/fae/error.h && \
	modulo -Mmodules --language=c    <modules/Fae/Buffer.module    		>include/fae/buffer.h && \
	modulo -Mmodules --language=c    <modules/Fae/Ratio.module    		>include/fae/ratio.h && \
	modulo -Mmodules --language=c    <modules/Fae/List.module    		>include/fae/list.h && \
	modulo -Mmodules --language=c    <modules/Fae/Set.module    		>include/fae/set.h && \
	modulo -Mmodules --language=c    <modules/Fae/Pair.module    		>include/fae/pair.h && \
	modulo -Mmodules --language=c    <modules/Fae/Plot.module    		>include/fae/plot.h && \
	modulo -Mmodules --language=c    <modules/Fae/Map.module    		>include/fae/map.h && \
	modulo -Mmodules --language=c    <modules/Fae/Graph.module    		>include/fae/graph.h && \
	modulo -Mmodules --language=c    <modules/Fae/PriorityQueue.module		>include/fae/priority_queue.h && \
	modulo -Mmodules --language=c    <modules/Fae/MidiMsg.module    		>include/fae/midi_msg.h && \
	modulo -Mmodules --language=c    <modules/Fae/Time.module    		>include/fae/time.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor.module  		>include/fae/processor.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Unary.module  	>include/fae/processor/unary.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Binary.module 	>include/fae/processor/binary.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Split.module  	>include/fae/processor/split.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Delay.module  	>include/fae/processor/delay.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Loop.module		>include/fae/processor/loop.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Seq.module 		>include/fae/processor/seq.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/Par.module 		>include/fae/processor/par.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/External/Vst.module		>include/fae/processor/external/vst.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/External/AudioUnit.module	>include/fae/processor/external/audio_unit.h && \
	modulo -Mmodules --language=c    <modules/Fae/Processor/External/FluidSynth.module	>include/fae/processor/external/fluid_synth.h && \
	modulo -Mmodules --language=c    <modules/Fae/Signal.module  		>include/fae/signal.h && \
	modulo -Mmodules --language=c    <modules/Fae/Type.module  			>include/fae/type.h && \
	modulo -Mmodules --language=c    <modules/Fae/Device.module			>include/fae/device.h && \
	modulo -Mmodules --language=c    <modules/Fae/Device/Buffer.module		>include/fae/device/buffer.h && \
	modulo -Mmodules --language=c    <modules/Fae/Device/File.module    	>include/fae/device/file.h && \
	modulo -Mmodules --language=c    <modules/Fae/Device/Audio.module    	>include/fae/device/audio.h && \
	modulo -Mmodules --language=c    <modules/Fae/Device/Midi.module    	>include/fae/device/midi.h && \
	modulo -Mmodules --language=c    <modules/Fae/Scheduler.module  		>include/fae/scheduler.h && \
	modulo -Mmodules --language=c    <modules/Fae/Message.module 		>include/fae/message.h;

lisp: fix-lisp
gen-lisp:
	modulo -Mmodules --language=lisp <modules/Fae.module    			>bindings/lisp/fae.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/AudioEngine.module    	>bindings/lisp/fae/audio-engine.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic.module    		>bindings/lisp/fae/atomic.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic/Queue.module       	>bindings/lisp/fae/atomic/queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic/Stack.module       	>bindings/lisp/fae/atomic/stack.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic/RingBuffer.module  	>bindings/lisp/fae/atomic/ring-buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Thread.module    		>bindings/lisp/fae/thread.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/System.module    		>bindings/lisp/fae/system.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/System/Directory.module    	>bindings/lisp/fae/system/directory.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/System/Event.module    	>bindings/lisp/fae/system/event.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Dynamic.module    		>bindings/lisp/fae/dynamic.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Event.module    		>bindings/lisp/fae/event.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Std.module    		>bindings/lisp/fae/std.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/String.module    		>bindings/lisp/fae/string.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/String/List.module    	>bindings/lisp/fae/string/list.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Error.module    		>bindings/lisp/fae/error.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Buffer.module    		>bindings/lisp/fae/buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Ratio.module    		>bindings/lisp/fae/ratio.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/List.module    		>bindings/lisp/fae/list.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Set.module    		>bindings/lisp/fae/set.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Pair.module    		>bindings/lisp/fae/pair.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Plot.module    		>bindings/lisp/fae/plot.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Map.module    		>bindings/lisp/fae/map.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Graph.module    		>bindings/lisp/fae/graph.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/PriorityQueue.module		>bindings/lisp/fae/priority-queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/MidiMsg.module    		>bindings/lisp/fae/midi_msg.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Time.module    		>bindings/lisp/fae/time.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Processor.module  		>bindings/lisp/fae/processor.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Processor/External/AudioUnit.module	>bindings/lisp/fae/processor/external/audio-unit.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Processor/External/Vst.module		>bindings/lisp/fae/processor/external/vst.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Processor/External/FluidSynth.module	>bindings/lisp/fae/processor/external/fluid-synth.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Signal.module  		>bindings/lisp/fae/signal.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Type.module  			>bindings/lisp/fae/type.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Device.module			>bindings/lisp/fae/device.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Device/Buffer.module		>bindings/lisp/fae/device/buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Device/File.module    	>bindings/lisp/fae/device/file.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Device/Audio.module    	>bindings/lisp/fae/device/audio.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Device/Midi.module    	>bindings/lisp/fae/device/midi.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Scheduler.module  		>bindings/lisp/fae/scheduler.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Message.module 		>bindings/lisp/fae/message.lisp;  

fix-lisp: gen-lisp
	perl -i -pe 's/\(defctype ptr/; (defctype ptr/g' bindings/lisp/fae.lisp

clean-lisp:
	find bindings -name '*.lisp' | xargs rm -v;