
EXECUTABLE=flp22-log

SRC=$(wildcard *.pl)

$(EXECUTABLE): $(SRC)
	swipl -q -g main -o $(EXECUTABLE) -c $(SRC)
