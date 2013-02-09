
all:	c
bindings: lisp

c:
	modulo -Mmodules --language=c    <modules/Doremir.module    			>include/doremir.h && \
	modulo -Mmodules --language=c    <modules/Doremir/AudioEngine.module    	>include/doremir/audio_engine.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic.module    		>include/doremir/atomic.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic/Queue.module       	>include/doremir/atomic/queue.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic/Stack.module       	>include/doremir/atomic/stack.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic/RingBuffer.module  	>include/doremir/atomic/ring_buffer.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Directory.module    		>include/doremir/directory.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Dynamic.module    		>include/doremir/dynamic.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Event.module    		>include/doremir/event.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Thread.module    		>include/doremir/thread.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Thread/Future.module    	>include/doremir/thread/future.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Thread/Improving.module    	>include/doremir/thread/improving.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Std.module    		>include/doremir/std.h && \
	modulo -Mmodules --language=c    <modules/Doremir/String.module    		>include/doremir/string.h && \
	modulo -Mmodules --language=c    <modules/Doremir/String/List.module    	>include/doremir/string/list.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Error.module    		>include/doremir/error.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Buffer.module    		>include/doremir/buffer.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Ratio.module    		>include/doremir/ratio.h && \
	modulo -Mmodules --language=c    <modules/Doremir/List.module    		>include/doremir/list.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Set.module    		>include/doremir/set.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Pair.module    		>include/doremir/pair.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Plot.module    		>include/doremir/plot.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Map.module    		>include/doremir/map.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Graph.module    		>include/doremir/graph.h && \
	modulo -Mmodules --language=c    <modules/Doremir/PriorityQueue.module		>include/doremir/priority_queue.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Midi.module    		>include/doremir/midi.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Time.module    		>include/doremir/time.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor.module  		>include/doremir/processor.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Unary.module  	>include/doremir/processor/unary.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Binary.module 	>include/doremir/processor/binary.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Split.module  	>include/doremir/processor/split.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Delay.module  	>include/doremir/processor/delay.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Loop.module		>include/doremir/processor/loop.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Seq.module 		>include/doremir/processor/seq.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Par.module 		>include/doremir/processor/par.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/External/Vst.module		>include/doremir/processor/external/vst.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/External/AudioUnit.module	>include/doremir/processor/external/audio_unit.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/External/FluidSynth.module	>include/doremir/processor/external/fluid_synth.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Signal.module  		>include/doremir/signal.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Type.module  			>include/doremir/type.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device.module			>include/doremir/device.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/Buffer.module		>include/doremir/device/buffer.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/File.module    	>include/doremir/device/file.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/Audio.module    	>include/doremir/device/audio.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/Midi.module    	>include/doremir/device/midi.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Scheduler.module  		>include/doremir/scheduler.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Message.module 		>include/doremir/message.h;

lisp: fix-lisp
gen-lisp:
	modulo -Mmodules --language=lisp <modules/Doremir.module    			>bindings/lisp/doremir.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/AudioEngine.module    	>bindings/lisp/doremir/audio-engine.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Atomic.module    		>bindings/lisp/doremir/atomic.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Atomic/Queue.module       	>bindings/lisp/doremir/atomic/queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Atomic/Stack.module       	>bindings/lisp/doremir/atomic/stack.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Atomic/RingBuffer.module  	>bindings/lisp/doremir/atomic/ring-buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Thread.module    		>bindings/lisp/doremir/thread.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Thread/Future.module    	>bindings/lisp/doremir/thread/future.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Thread/Improving.module    	>bindings/lisp/doremir/thread/improving.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Directory.module    		>bindings/lisp/doremir/directory.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Dynamic.module    		>bindings/lisp/doremir/dynamic.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Event.module    		>bindings/lisp/doremir/event.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Std.module    		>bindings/lisp/doremir/std.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/String.module    		>bindings/lisp/doremir/string.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/String/List.module    	>bindings/lisp/doremir/string/list.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Error.module    		>bindings/lisp/doremir/error.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Buffer.module    		>bindings/lisp/doremir/buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Ratio.module    		>bindings/lisp/doremir/ratio.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/List.module    		>bindings/lisp/doremir/list.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Set.module    		>bindings/lisp/doremir/set.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Pair.module    		>bindings/lisp/doremir/pair.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Plot.module    		>bindings/lisp/doremir/plot.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Map.module    		>bindings/lisp/doremir/map.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Graph.module    		>bindings/lisp/doremir/graph.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/PriorityQueue.module		>bindings/lisp/doremir/priority-queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Midi.module    		>bindings/lisp/doremir/midi.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Time.module    		>bindings/lisp/doremir/time.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Processor.module  		>bindings/lisp/doremir/processor.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Processor/External/AudioUnit.module	>bindings/lisp/doremir/processor/external/audio-unit.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Processor/External/Vst.module		>bindings/lisp/doremir/processor/external/vst.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Processor/External/FluidSynth.module	>bindings/lisp/doremir/processor/external/fluid-synth.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Signal.module  		>bindings/lisp/doremir/signal.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Type.module  			>bindings/lisp/doremir/type.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Device.module			>bindings/lisp/doremir/device.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Device/Buffer.module		>bindings/lisp/doremir/device/buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Device/File.module    	>bindings/lisp/doremir/device/file.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Device/Audio.module    	>bindings/lisp/doremir/device/audio.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Device/Midi.module    	>bindings/lisp/doremir/device/midi.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Scheduler.module  		>bindings/lisp/doremir/scheduler.lisp && \
	modulo -Mmodules --language=lisp <modules/Doremir/Message.module 		>bindings/lisp/doremir/message.lisp;  

fix-lisp: gen-lisp
	perl -i -pe 's/\(defctype ptr/; (defctype ptr/g' bindings/lisp/doremir.lisp

clean-lisp:
	find bindings -name '*.lisp' | xargs rm -v;