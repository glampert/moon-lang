
# TODO:
# Should have two different build settings:
# - debug
# - release
# Enable higher optimizations on release and maybe also LTO
#
# Consider splitting the source/ in:
# lib/
# cli/
# Build lib as a static library and cli as a command-line tool.
# This makefile should allows building one or both.
#
# We should probably also have a separate test binary that runs
# all the test script in one go, so we can run 'make test' from the cmdline
#
# A separate Windows build (VS project)
#

#------------------------------------------------

# Define 'VERBOSE' to get the full console output.
# Otherwise print a short message for each rule.
ifndef VERBOSE
  QUIET = @
endif # VERBOSE

#------------------------------------------------
# Macros / env:

BIN_TARGET      = moon
GENERATED_DIR   = generated
OBJ_DIR         = obj
SOURCE_DIR      = source
MKDIR_CMD       = mkdir -p
LEX_SRC         = $(wildcard $(SOURCE_DIR)/*.lxx)
BISON_SRC       = $(wildcard $(SOURCE_DIR)/*.yxx)
CPP_SRC         = $(wildcard $(SOURCE_DIR)/*.cpp)
LEX_GENERATED   = $(addprefix $(GENERATED_DIR)/, $(notdir $(patsubst %.lxx, %.cpp, $(LEX_SRC))))
BISON_GENERATED = $(addprefix $(GENERATED_DIR)/, $(notdir $(patsubst %.yxx, %.cpp, $(BISON_SRC))))
SRC_FILES       = $(BISON_GENERATED) $(LEX_GENERATED) $(CPP_SRC)
OBJ_FILES       = $(addprefix $(OBJ_DIR)/, $(patsubst %.cpp, %.o, $(SRC_FILES)))

DEBUG_FLAGS = -g                 \
              -DDEBUG=1          \
              -D_DEBUG=1         \
              -D_LIBCPP_DEBUG=0  \
              -D_LIBCPP_DEBUG2=0 \
              -DMOON_DEBUG=1

GLOBAL_DEFINES = -DMOON_ENABLE_ASSERT=1 \
                 -DMOON_PRINT_USE_ANSI_COLOR_CODES=1 \
                 -DMOON_PRINT_RT_OBJECT_FLAGS=1

WARNS_USED = -Wall                   \
             -Wextra                 \
             -Wsequence-point        \
             -Wdisabled-optimization \
             -Wuninitialized         \
             -Wshadow                \
             -Wformat=2              \
             -Winit-self

WARNS_IGNORED = -Wno-deprecated-register \
                -Wno-unused-function     \
                -Wno-sign-compare

CXXFLAGS += -std=c++11           \
            $(WARNS_USED)        \
            $(WARNS_IGNORED)     \
            $(DEBUG_FLAGS)       \
            $(GLOBAL_DEFINES)    \
            -I.                  \
            -I/usr/local/include \
            -I$(GENERATED_DIR)/  \
            -I$(SOURCE_DIR)/

# For ad hoc static analysis with Clang, uncomment the following:
#CXX += --analyze -Xanalyzer -analyzer-output=text

#------------------------------------------------
# CPlusPlus rules:

all: $(BIN_TARGET)
	$(QUIET) strip $(BIN_TARGET)

$(BIN_TARGET): $(OBJ_FILES)
	@echo "-> Linking ..."
	$(QUIET) $(CXX) $(CXXFLAGS) -o $(BIN_TARGET) $(OBJ_FILES) $(LIB_FILES)

$(OBJ_FILES): $(OBJ_DIR)/%.o: %.cpp
	@echo "-> Compiling" $< "..."
	$(QUIET) $(MKDIR_CMD) $(dir $@)
	$(QUIET) $(CXX) $(CXXFLAGS) -c $< -o $@

#------------------------------------------------
# Bison rules:

# Preserves the generated source files
.PRECIOUS: $(GENERATED_DIR)/%.cpp

$(GENERATED_DIR)/%.cpp: $(SOURCE_DIR)/%.yxx
	@echo "-> Running Bison for" $< "..."
	$(QUIET) bison -o $@ -d $<

#------------------------------------------------
# Flex rules:

# Preserves the generated source files
.PRECIOUS: $(GENERATED_DIR)/%.cpp

$(GENERATED_DIR)/%.cpp: $(SOURCE_DIR)/%.lxx
	@echo "-> Running Flex for" $< "..."
	$(QUIET) flex -t $< > $@

#------------------------------------------------
# make clean:

clean:
	@echo "-> Cleaning ..."
	$(QUIET) rm -f $(BIN_TARGET) $(LEX_GENERATED) $(BISON_GENERATED)
	$(QUIET) rm -f $(GENERATED_DIR)/*.hh $(GENERATED_DIR)/*.hpp
	$(QUIET) rm -rf $(OBJ_DIR)

