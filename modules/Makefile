
all:	c
bindings: lisp

c:
	mkdir -p include/fae
	mkdir -p include/fae/atomic
	mkdir -p include/fae/device
	mkdir -p include/fae/midi
	mkdir -p include/fae/processor
	mkdir -p include/fae/processor/external
	mkdir -p include/fae/string
	mkdir -p include/fae/system
	mkdir -p include/fae/thread
	modulo -Mmodules --language=c    <modules/Fae.module    		>include/fae.h && \
	modulo -Mmodules --language=c    <modules/Fae/Fae.module    		>include/fae/fae.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic.module    		>include/fae/atomic.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic/Queue.module       >include/fae/atomic/queue.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic/Stack.module       >include/fae/atomic/stack.h && \
	modulo -Mmodules --language=c    <modules/Fae/Atomic/RingBuffer.module  >include/fae/atomic/ring_buffer.h && \
	modulo -Mmodules --language=c    <modules/Fae/System.module    		>include/fae/system.h && \
	modulo -Mmodules --language=c    <modules/Fae/System/Directory.module   >include/fae/system/directory.h && \
	modulo -Mmodules --language=c    <modules/Fae/System/Event.module    	>include/fae/system/event.h && \
	modulo -Mmodules --language=c    <modules/Fae/Dynamic.module    	>include/fae/dynamic.h && \
	modulo -Mmodules --language=c    <modules/Fae/Event.module    		>include/fae/event.h && \
	modulo -Mmodules --language=c    <modules/Fae/Thread.module    		>include/fae/thread.h && \
	modulo -Mmodules --language=c    <modules/Fae/Std.module    		>include/fae/std.h && \
	modulo -Mmodules --language=c    <modules/Fae/String.module    		>include/fae/string.h && \
	modulo -Mmodules --language=c    <modules/Fae/Error.module    		>include/fae/error.h && \
	modulo -Mmodules --language=c    <modules/Fae/Buffer.module    		>include/fae/buffer.h && \
	modulo -Mmodules --language=c    <modules/Fae/Ratio.module    		>include/fae/ratio.h && \
	modulo -Mmodules --language=c    <modules/Fae/List.module    		>include/fae/list.h && \
	modulo -Mmodules --language=c    <modules/Fae/Set.module    		>include/fae/set.h && \
	modulo -Mmodules --language=c    <modules/Fae/Pair.module    		>include/fae/pair.h && \
	modulo -Mmodules --language=c    <modules/Fae/Plot.module    		>include/fae/plot.h && \
	modulo -Mmodules --language=c    <modules/Fae/Map.module    		>include/fae/map.h && \
	modulo -Mmodules --language=c    <modules/Fae/Graph.module    		>include/fae/graph.h && \
	modulo -Mmodules --language=c    <modules/Fae/PriorityQueue.module	>include/fae/priority_queue.h && \
	modulo -Mmodules --language=c    <modules/Fae/Midi/Message.module    	>include/fae/midi/message.h && \
	modulo -Mmodules --language=c    <modules/Fae/Time.module    		>include/fae/time.h && \
	modulo -Mmodules --language=c    <modules/Fae/Signal.module  		>include/fae/signal.h && \
	modulo -Mmodules --language=c    <modules/Fae/Type.module  		>include/fae/type.h && \
	modulo -Mmodules --language=c    <modules/Fae/Audio.module    		>include/fae/audio.h && \
	modulo -Mmodules --language=c    <modules/Fae/Midi.module    		>include/fae/midi.h

lisp: fix-lisp
gen-lisp:
	mkdir -p bindings/lisp/fae
	mkdir -p bindings/lisp/fae/atomic
	mkdir -p bindings/lisp/fae/device
	mkdir -p bindings/lisp/fae/midi
	mkdir -p bindings/lisp/fae/processor
	mkdir -p bindings/lisp/fae/processor/external
	mkdir -p bindings/lisp/fae/string
	mkdir -p bindings/lisp/fae/system
	mkdir -p bindings/lisp/fae/thread
	modulo -Mmodules --language=lisp <modules/Fae.module    		>bindings/lisp/fae.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Fae.module    		>bindings/lisp/fae/fae.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic.module    		>bindings/lisp/fae/atomic.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic/Queue.module       >bindings/lisp/fae/atomic/queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic/Stack.module       >bindings/lisp/fae/atomic/stack.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Atomic/RingBuffer.module  >bindings/lisp/fae/atomic/ring-buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Thread.module    		>bindings/lisp/fae/thread.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/System.module    		>bindings/lisp/fae/system.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/System/Directory.module   >bindings/lisp/fae/system/directory.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/System/Event.module    	>bindings/lisp/fae/system/event.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Dynamic.module    	>bindings/lisp/fae/dynamic.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Event.module    		>bindings/lisp/fae/event.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Std.module    		>bindings/lisp/fae/std.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/String.module    		>bindings/lisp/fae/string.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Error.module    		>bindings/lisp/fae/error.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Buffer.module    		>bindings/lisp/fae/buffer.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Ratio.module    		>bindings/lisp/fae/ratio.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/List.module    		>bindings/lisp/fae/list.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Set.module    		>bindings/lisp/fae/set.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Pair.module    		>bindings/lisp/fae/pair.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Plot.module    		>bindings/lisp/fae/plot.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Map.module    		>bindings/lisp/fae/map.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Graph.module    		>bindings/lisp/fae/graph.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/PriorityQueue.module	>bindings/lisp/fae/priority-queue.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Midi/Message.module    	>bindings/lisp/fae/midi/message.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Time.module    		>bindings/lisp/fae/time.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Signal.module  		>bindings/lisp/fae/signal.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Type.module  		>bindings/lisp/fae/type.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Audio.module    		>bindings/lisp/fae/audio.lisp && \
	modulo -Mmodules --language=lisp <modules/Fae/Midi.module    		>bindings/lisp/fae/midi.lisp

fix-lisp: gen-lisp
	perl -i -pe 's/\(defctype ptr/; (defctype ptr/g' bindings/lisp/fae.lisp

clean-lisp:
	find bindings -name '*.lisp' | xargs rm -v;