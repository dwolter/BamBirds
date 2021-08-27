CC := swipl-ld # This is the main compiler
# CC := clang --analyze # and comment out the linker last line for sanity
SRCDIR := src
BUILDDIR := build
BINDIR := bin
TARGET := behind_the_corner.so
 
SRCEXT := cpp
SOURCES := $(shell find $(SRCDIR) -type f -name '*.$(SRCEXT)')
OBJECTS := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SOURCES:.$(SRCEXT)=.o))
CFLAGS := -g # -Wall
LIB := -L lib -lboost_system # -lX11
INC := -Iinclude

$(TARGET): $(SOURCES)
	@echo " Linking..."
	@mkdir -p $(BINDIR)
	@echo " $(CC) $(CFLAGS) $(INC) -shared -o ${BINDIR}/$(TARGET) $(LIB) $(SOURCES)"; $(CC) $(CFLAGS) $(INC) -shared -o ${BINDIR}/$(TARGET) $(LIB) $^

# $(BUILDDIR)/%.o: $(SRCDIR)/%.$(SRCEXT)
# 	@mkdir -p $(BUILDDIR)
# 	@echo " $(CC) $(CFLAGS) $(INC) -std=c++11 -fPIC -c -o $@ $<"; $(CC) $(CFLAGS) $(INC) -std=c++11 -fPIC -c -o $@ $<

clean:
	@echo " Cleaning..."; 
	@echo " $(RM) -r $(BUILDDIR) $(BINDIR)"; $(RM) -r $(BUILDDIR) $(BINDIR)

# Tests
tester:
	$(CC) $(CFLAGS) test/tester.cpp $(INC) $(LIB) -o bin/tester

.PHONY: clean



# build : behind_the_corner.cpp scene.cpp utils.cpp simulation.cpp 
# 		swipl-ld -shared -o behind_the_corner.so behind_the_corner.cpp \
# 			scene.cpp utils.cpp simulation.cpp

# clean :
# 		rm behind_the_corner.so