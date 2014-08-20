include ../config.mk

OBJS = boxing.o io.o main.o operators.o utility.o vm.o
HDRS = boxing.h callbacks.h exceptions.h io.h lambdas.h main.h mem.h operators.h types.h utility.h vm.h
CPPFLAGS:=-fPIC $(CPPFLAGS:)
CPPFLAGS += $(GMP_INCLUDE_DIR) $(GMP) -DIDRIS_TARGET_OS="\"$(OS)\""
CPPFLAGS += "-std=c++11"
CPPFLAGS += -DIDRIS_TARGET_TRIPLE="\"$(MACHINE)\""

LIBTARGET = libidris_cpp_rts.a

build: $(LIBTARGET) $(DYLIBTARGET)

$(LIBTARGET) : $(OBJS)
	ar r $(LIBTARGET) $(OBJS)
	ranlib $(LIBTARGET)

install :
	mkdir -p $(TARGET)
	install $(LIBTARGET) $(HDRS) $(TARGET)

clean :
	rm -f $(OBJS) $(LIBTARGET) $(DYLIBTARGET)

.PHONY: build install clean
