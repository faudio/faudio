
all: concept

concept:
	cat concepts.dot \
		| cpp \
		| dot -Tpdf \
		> concepts.pdf \
		&& open concepts.pdf;

interaction:
	cat interactions.dot \
		| cpp \
		| dot -Tpdf \
		> interactions.pdf \
		&& open interactions.pdf;