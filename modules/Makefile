
all:	gen

gen:
	modulo --language=c    <modules/Atomic.module    		>modules2/Atomic.h; \
	modulo --language=c    <modules/Atomic/Queue.module       	>modules2/Atomic/Queue.h; \
	modulo --language=c    <modules/Atomic/Stack.module       	>modules2/Atomic/Stack.h; \
	modulo --language=c    <modules/Atomic/RingBuffer.module  	>modules2/Atomic/RingBuffer.h; \
	modulo --language=c    <modules/Thread.module    		>modules2/Thread.h;
	modulo --language=c    <modules/Thread/Future.module    	>modules2/Thread/Future.h;
	modulo --language=c    <modules/Thread/Improving.module    	>modules2/Thread/Improving.h;

	modulo --language=c    <modules/Chase.module    		>modules2/Chase.h;
	modulo --language=c    <modules/List.module    			>modules2/List.h;
	modulo --language=c    <modules/Set.module    			>modules2/Set.h;
	modulo --language=c    <modules/Pair.module    			>modules2/Pair.h;
	modulo --language=c    <modules/Map.module    			>modules2/Map.h;

	modulo --language=c    <modules/Audio/Types.module    		>modules2/Audio/Types.h;
	modulo --language=c    <modules/Audio/Time.module    		>modules2/Audio/Time.h;

	modulo --language=c    <modules/Audio/Processor.module  	>modules2/Audio/Processor.h;
	modulo --language=c    <modules/Audio/Processor/Unary.module  	>modules2/Audio/Processor/Unary.h;
	modulo --language=c    <modules/Audio/Processor/Binary.module  	>modules2/Audio/Processor/Binary.h;
	modulo --language=c    <modules/Audio/Processor/Ternary.module  >modules2/Audio/Processor/Ternary.h;
	modulo --language=c    <modules/Audio/Processor/Delay.module  	>modules2/Audio/Processor/Split.h;
	modulo --language=c    <modules/Audio/Processor/Split.module  	>modules2/Audio/Processor/Delay.h;
	modulo --language=c    <modules/Audio/Processor/Sequence.module >modules2/Audio/Processor/Sequence.h;
	modulo --language=c    <modules/Audio/Processor/Parallel.module >modules2/Audio/Processor/Parallel.h;
	modulo --language=c    <modules/Audio/Processor/Loop.module	>modules2/Audio/Processor/Loop.h;

	modulo --language=c    <modules/Audio/Device.module    		>modules2/Audio/Device.h;
	modulo --language=c    <modules/Audio/Device/Buffer.module    		>modules2/Audio/Device/Buffer.h;
	modulo --language=c    <modules/Audio/Device/File.module    		>modules2/Audio/Device/File.h;
	modulo --language=c    <modules/Audio/Device/Audio.module    		>modules2/Audio/Device/Audio.h;
	modulo --language=c    <modules/Audio/Device/Midi.module    		>modules2/Audio/Device/Midi.h;


	modulo --language=c    <modules/Audio/Scheduler.module  	>modules2/Audio/Scheduler.h;
	modulo --language=c    <modules/Audio/Dispatcher.module 	>modules2/Audio/Dispatcher.h;



doc:    gen
	doxygen modules2/Doxyfile

clean:
	rm modules2/*.h; \
	rm modules2/*.lisp;