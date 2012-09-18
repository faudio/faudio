
all: concepts interactions

concepts:
	cat concepts.dot \
		| cpp \
		| dot -Tpdf \
		> concepts.pdf \
		&& open concepts.pdf;

interactions:
	cat interactions.dot \
		| cpp \
		| dot -Tpdf \
		> interactions.pdf \
		&& open interactions.pdf;