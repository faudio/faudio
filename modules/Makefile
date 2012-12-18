
all:	gen

gen:
	modulo -Mmodules --language=c    <modules/Doremir.module    			>include/doremir.h && \
	modulo -Mmodules --language=c    <modules/Doremir/AudioEngine.module    	>include/doremir/audio_engine.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic.module    		>include/doremir/atomic.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic/Queue.module       	>include/doremir/atomic/queue.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic/Stack.module       	>include/doremir/atomic/stack.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Atomic/RingBuffer.module  	>include/doremir/atomic/ring_buffer.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Thread.module    		>include/doremir/thread.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Thread/Future.module    	>include/doremir/thread/future.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Thread/Improving.module    	>include/doremir/thread/improving.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Std.module    		>include/doremir/std.h && \
	modulo -Mmodules --language=c    <modules/Doremir/String.module    		>include/doremir/string.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Buffer.module    		>include/doremir/buffer.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Ratio.module    		>include/doremir/ratio.h && \
	modulo -Mmodules --language=c    <modules/Doremir/List.module    		>include/doremir/list.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Set.module    		>include/doremir/set.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Pair.module    		>include/doremir/pair.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Map.module    		>include/doremir/map.h && \
	modulo -Mmodules --language=c    <modules/Doremir/PriorityQueue.module		>include/doremir/priority_queue.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Midi.module    		>include/doremir/midi.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Time.module    		>include/doremir/time.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor.module  		>include/doremir/processor.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Unary.module  	>include/doremir/processor/unary.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Binary.module 	>include/doremir/processor/binary.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Delay.module  	>include/doremir/processor/split.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Split.module  	>include/doremir/processor/delay.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Sequence.module 	>include/doremir/processor/sequence.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Parallel.module 	>include/doremir/processor/parallel.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/Loop.module		>include/doremir/processor/loop.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/AudioUnit.module	>include/doremir/processor/audio_unit.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/VST.module		>include/doremir/processor/vst.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Processor/FluidSynth.module	>include/doremir/processor/fluid_synth.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/Buffer.module		>include/doremir/device/buffer.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/File.module    	>include/doremir/device/file.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/Audio.module    	>include/doremir/device/audio.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Device/Midi.module    	>include/doremir/device/midi.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Scheduler.module  		>include/doremir/scheduler.h && \
	modulo -Mmodules --language=c    <modules/Doremir/Dispatcher.module 		>include/doremir/dispatcher.h;


doc:    gen
	doxygen doc/Doxyfile

clean:
	rm include/*.h; \
	rm bindings/lsp/*.lisp;