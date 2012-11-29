
all:	gen

gen:
	modulo --language=c    <modules/Atomic.module    		>inc/doremir/atomic.h; \
	modulo --language=c    <modules/Atomic/Queue.module       	>inc/doremir/atomic/queue.h; \
	modulo --language=c    <modules/Atomic/Stack.module       	>inc/doremir/atomic/stack.h; \
	modulo --language=c    <modules/Atomic/RingBuffer.module  	>inc/doremir/atomic/ring_buffer.h; \
	modulo --language=c    <modules/Thread.module    		>inc/doremir/thread.h;
	modulo --language=c    <modules/Thread/Future.module    	>inc/doremir/thread/future.h;
	modulo --language=c    <modules/Thread/Improving.module    	>inc/doremir/thread/improving.h;
                                                                                          
	modulo --language=c    <modules/Chase.module    		>inc/doremir/chase.h;
	modulo --language=c    <modules/List.module    			>inc/doremir/list.h; 
	modulo --language=c    <modules/Set.module    			>inc/doremir/set.h;  
	modulo --language=c    <modules/Pair.module    			>inc/doremir/pair.h; 
	modulo --language=c    <modules/Map.module    			>inc/doremir/map.h;  
                                                                                          
	modulo --language=c    <modules/Audio/Types.module    		>inc/doremir/audio/types.h;
	modulo --language=c    <modules/Audio/Time.module    		>inc/doremir/audio/time.h;
                                                                                         
	modulo --language=c    <modules/Audio/Processor.module  	>inc/doremir/audio/processor.h;
	modulo --language=c    <modules/Audio/Processor/Unary.module  	>inc/doremir/audio/processor/unary.h;
	modulo --language=c    <modules/Audio/Processor/Binary.module  	>inc/doremir/audio/processor/binary.h;
	modulo --language=c    <modules/Audio/Processor/Ternary.module  >inc/doremir/audio/processor/ternary.h;
	modulo --language=c    <modules/Audio/Processor/Delay.module  	>inc/doremir/audio/processor/split.h;
	modulo --language=c    <modules/Audio/Processor/Split.module  	>inc/doremir/audio/processor/delay.h;
	modulo --language=c    <modules/Audio/Processor/Sequence.module >inc/doremir/audio/processor/sequence.h;
	modulo --language=c    <modules/Audio/Processor/Parallel.module >inc/doremir/audio/processor/parallel.h;
	modulo --language=c    <modules/Audio/Processor/Loop.module	>inc/doremir/audio/processor/loop.h;
                                                                                                   
	modulo --language=c    <modules/Audio/Device.module    		>inc/doremir/audio/device.h;
	modulo --language=c    <modules/Audio/Device/Buffer.module	>inc/doremir/audio/device/buffer.h;
	modulo --language=c    <modules/Audio/Device/File.module    	>inc/doremir/audio/device/file.h;
	modulo --language=c    <modules/Audio/Device/Audio.module    	>inc/doremir/audio/device/audio.h;
	modulo --language=c    <modules/Audio/Device/Midi.module    	>inc/doremir/audio/device/midi.h;
                                                                                                
	modulo --language=c    <modules/Audio/Scheduler.module  	>inc/doremir/audio/scheduler.h;
	modulo --language=c    <modules/Audio/Dispatcher.module 	>inc/doremir/audio/dispatcher.h;
                                                                                         


doc:    gen
	doxygen inc/Doxyfile

clean:
	rm inc/*.h; \
	rm inc/*.lisp;