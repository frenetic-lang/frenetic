.PHONY: all tar

INSTALL_DIR=$(shell which ocaml | cut -d / -f 0-4)
FILES   = version.ml pack.ml
TARGET  = ocp-pack

FILES2  = version.ml split.ml
TARGET2 = ocp-split

all: $(FILES) $(FILES2)
	ocamlopt $(FILES) -o $(TARGET)
	ocamlopt $(FILES2) -o $(TARGET2)

install:
	cp $(TARGET) $(TARGET2) $(INSTALL_DIR)

uninstall:
	rm -f $(INSTALL_DIR)/$(TARGET) $(INSTALL_DIR)/$(TARGET2)

clean:
	rm -f *.o *.cmi *.cmx
