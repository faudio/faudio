
all:	c
bindings: lisp

c:
	mkdir -p include/fa
	mkdir -p include/fa/atomic
	mkdir -p include/fa/device
	mkdir -p include/fa/midi
	mkdir -p include/fa/processor
	mkdir -p include/fa/processor/external
	mkdir -p include/fa/string
	mkdir -p include/fa/system
	mkdir -p include/fa/thread
	modulo -Mmodules --language=c    <modules/Fa.module    		>include/fa.h && \
	modulo -Mmodules --language=c    <modules/Fa/Fa.module    		>include/fa/fa.h && \
	modulo -Mmodules --language=c    <modules/Fa/Atomic.module    		>include/fa/atomic.h && \
	modulo -Mmodules --language=c    <modules/Fa/Atomic/Queue.module       >include/fa/atomic/queue.h && \
	modulo -Mmodules --language=c    <modules/Fa/Atomic/Stack.module       >include/fa/atomic/stack.h && \
	modulo -Mmodules --language=c    <modules/Fa/Atomic/RingBuffer.module  >include/fa/atomic/ring_buffer.h && \
	modulo -Mmodules --language=c    <modules/Fa/System.module    		>include/fa/system.h && \
	modulo -Mmodules --language=c    <modules/Fa/System/Directory.module   >include/fa/system/directory.h && \
	modulo -Mmodules --language=c    <modules/Fa/System/Event.module    	>include/fa/system/event.h && \
	modulo -Mmodules --language=c    <modules/Fa/Device.module    	>include/fa/device.h && \
	modulo -Mmodules --language=c    <modules/Fa/Dynamic.module    	>include/fa/dynamic.h && \
	modulo -Mmodules --language=c    <modules/Fa/Thread.module    		>include/fa/thread.h && \
	modulo -Mmodules --language=c    <modules/Fa/Std.module    		>include/fa/std.h && \
	modulo -Mmodules --language=c    <modules/Fa/String.module    		>include/fa/string.h && \
	modulo -Mmodules --language=c    <modules/Fa/Error.module    		>include/fa/error.h && \
	modulo -Mmodules --language=c    <modules/Fa/Buffer.module    		>include/fa/buffer.h && \
	modulo -Mmodules --language=c    <modules/Fa/Ratio.module    		>include/fa/ratio.h && \
	modulo -Mmodules --language=c    <modules/Fa/List.module    		>include/fa/list.h && \
	modulo -Mmodules --language=c    <modules/Fa/Set.module    		>include/fa/set.h && \
	modulo -Mmodules --language=c    <modules/Fa/Pair.module    		>include/fa/pair.h && \
	modulo -Mmodules --language=c    <modules/Fa/Plot.module    		>include/fa/plot.h && \
	modulo -Mmodules --language=c    <modules/Fa/Map.module    		>include/fa/map.h && \
	modulo -Mmodules --language=c    <modules/Fa/Graph.module    		>include/fa/graph.h && \
	modulo -Mmodules --language=c    <modules/Fa/PriorityQueue.module	>include/fa/priority_queue.h && \
	modulo -Mmodules --language=c    <modules/Fa/Midi/Message.module    	>include/fa/midi/message.h && \
	modulo -Mmodules --language=c    <modules/Fa/Time.module    		>include/fa/time.h && \
	modulo -Mmodules --language=c    <modules/Fa/Signal.module  		>include/fa/signal.h && \
	modulo -Mmodules --language=c    <modules/Fa/Type.module  		>include/fa/type.h && \
	modulo -Mmodules --language=c    <modules/Fa/Audio.module    		>include/fa/audio.h && \
	modulo -Mmodules --language=c    <modules/Fa/Midi.module    		>include/fa/midi.h

lisp: fix-lisp
gen-lisp:
	mkdir -p bindings/lisp/fa
	mkdir -p bindings/lisp/fa/atomic
	mkdir -p bindings/lisp/fa/device
	mkdir -p bindings/lisp/fa/midi
	mkdir -p bindings/lisp/fa/processor
	mkdir -p bindings/lisp/fa/processor/external
	mkdir -p bindings/lisp/fa/string
	mkdir -p bindings/lisp/fa/system
	mkdir -p bindings/lisp/fa/thread
	modulo -Mmodules --language=lisp <modules/Fa.module    		>bindings/lisp/fa.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Fa.module    		>bindings/lisp/fa/fa.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Atomic.module    		>bindings/lisp/fa/atomic.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Atomic/Queue.module       >bindings/lisp/fa/atomic/queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Atomic/Stack.module       >bindings/lisp/fa/atomic/stack.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Atomic/RingBuffer.module  >bindings/lisp/fa/atomic/ring-buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Thread.module    		>bindings/lisp/fa/thread.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/System.module    		>bindings/lisp/fa/system.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/System/Directory.module   >bindings/lisp/fa/system/directory.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/System/Event.module    	>bindings/lisp/fa/system/event.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Dynamic.module    	>bindings/lisp/fa/dynamic.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Std.module    		>bindings/lisp/fa/std.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/String.module    		>bindings/lisp/fa/string.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Error.module    		>bindings/lisp/fa/error.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Buffer.module    		>bindings/lisp/fa/buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Ratio.module    		>bindings/lisp/fa/ratio.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/List.module    		>bindings/lisp/fa/list.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Set.module    		>bindings/lisp/fa/set.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Pair.module    		>bindings/lisp/fa/pair.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Plot.module    		>bindings/lisp/fa/plot.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Map.module    		>bindings/lisp/fa/map.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Graph.module    		>bindings/lisp/fa/graph.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/PriorityQueue.module	>bindings/lisp/fa/priority-queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Midi/Message.module    	>bindings/lisp/fa/midi/message.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Time.module    		>bindings/lisp/fa/time.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Signal.module  		>bindings/lisp/fa/signal.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Type.module  		>bindings/lisp/fa/type.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Audio.module    		>bindings/lisp/fa/audio.lisp && \
	modulo -Mmodules --language=lisp <modules/Fa/Midi.module    		>bindings/lisp/fa/midi.lisp

fix-lisp: gen-lisp
	perl -i -pe 's/\(defctype ptr/; (defctype ptr/g' bindings/lisp/fa.lisp

clean-lisp:
	find bindings -name '*.lisp' | xargs rm -v;