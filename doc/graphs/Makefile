
all: device_states device_states2
show: all
	open doc/images/device_states.png; \
	open doc/graphs/device_states2.pdf;


# concepts:
# 	cat concepts.dot \
# 		| cpp \
# 		| dot -Tpdf \
# 		> concepts.pdf;
# 
# interactions:
# 	cat interactions.dot \
# 		| cpp \
# 		| dot -Tpdf \
# 		> interactions.pdf;
device_states:
	cat doc/graphs/device_states.dot \
		| cpp \
		| dot -Tpdf \
		> doc/graphs/device_states.pdf \
		&& convert \
			-density 300x300 -resize 25% \
			doc/graphs/device_states.pdf \
			doc/images/device_states.png;

device_states2:
	cat doc/graphs/device_states2.dot \
		| cpp \
		| dot -Tpdf \
		> doc/graphs/device_states2.pdf \
		&& convert \
 			-density 300x300 -resize 25% \
       			doc/graphs/device_states2.pdf \
			doc/images/device_states2.png;

