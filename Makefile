
#------------------------------------------------

# Available build rules/targets:
#
# - all (default):
#  $ make
#  Same as debug.
#
# - debug:
#  $ make debug
#  Enables debug symbols (-g) asserts and
#  a few other more costly runtime checks.
#
# - release:
#  $ make release
#  Build with optimizations turned on (-O3), no assertions,
#  no debug symbols and no additional runtime checks.
#
# - static_check:
#  $ make static_check
#  Runs Clang static analyzer on the source code.
#  This will not generate an executable or lib,
#  so the linking stage will fail.
#
# - ASan:
#  $ make asan
#  Build in debug mode and with Clang's address sanitizer.
#  See: http://clang.llvm.org/docs/AddressSanitizer.html
#
# Other global flags:
#
# - Setting the 'VERBOSE' variable causes the whole
#   commands to be printed. If this is not set,
#   a short summary is printed for each command
#   instead.
#

#------------------------------------------------
# Macros / env:

CLI_BIN         = moon
STATIC_LIB      = libMoon.a

GENERATED_DIR   = generated
OBJ_DIR         = obj
SOURCE_DIR      = source/lib
CLI_SRC         = source/cli/main.cpp

MKDIR_CMD       = mkdir -p
AR_CMD          = ar rcs
STRIP_CMD       = strip

LEX_SRC         = $(wildcard $(SOURCE_DIR)/*.lxx)
BISON_SRC       = $(wildcard $(SOURCE_DIR)/*.yxx)
CPP_SRC         = $(wildcard $(SOURCE_DIR)/*.cpp)

LEX_GENERATED   = $(addprefix $(GENERATED_DIR)/, $(notdir $(patsubst %.lxx, %.cpp, $(LEX_SRC))))
BISON_GENERATED = $(addprefix $(GENERATED_DIR)/, $(notdir $(patsubst %.yxx, %.cpp, $(BISON_SRC))))
SRC_FILES       = $(BISON_GENERATED) $(LEX_GENERATED) $(CPP_SRC)
OBJ_FILES       = $(addprefix $(OBJ_DIR)/, $(patsubst %.cpp, %.o, $(SRC_FILES)))

# Define 'VERBOSE' to get the full console output.
# Otherwise print a short message for each rule.
ifndef VERBOSE
  QUIET = @
endif # VERBOSE

# Additional release settings:
RELEASE_FLAGS = -O3                    \
                -DNDEBUG=1             \
                -DMOON_DEBUG=0         \
                -DMOON_ENABLE_ASSERT=0

# Additional debug settings:
DEBUG_FLAGS = -g                     \
              -DDEBUG=1              \
              -D_DEBUG=1             \
              -D_LIBCPP_DEBUG=0      \
              -D_LIBCPP_DEBUG2=0     \
              -DMOON_DEBUG=1         \
              -DMOON_ENABLE_ASSERT=1

# Paranoid warning flags:
WARNS_USED = -Wall                   \
             -Wextra                 \
             -Wsequence-point        \
             -Wdisabled-optimization \
             -Wuninitialized         \
             -Wshadow                \
             -Wformat=2              \
             -Winit-self             \
             -Wwrite-strings

# Some warnings generated by the Flex and Bison generated code that we can't fix:
WARNS_IGNORED = -Wno-deprecated-register \
                -Wno-unused-function     \
                -Wno-sign-compare

# Additional include commands:
INCLUDE_DIRS = -I.                  \
               -I/usr/local/include \
               -I$(GENERATED_DIR)/  \
               -I$(SOURCE_DIR)/

# Static analysis with Clang:
STATIC_CHECK_FLAGS = --analyze -Xanalyzer -analyzer-output=text

# Clang address sanitizer (use it with the debug target):
ASAN_FLAGS = -O1 -fno-omit-frame-pointer -fsanitize=address

# All flags combined:
CXXFLAGS += -std=c++11       \
            $(WARNS_USED)    \
            $(WARNS_IGNORED) \
            $(INCLUDE_DIRS)

#------------------------------------------------
# CPlusPlus rules:

# Default rule. Same as debug.
all: CXXFLAGS += $(DEBUG_FLAGS)
all: common_rule
	@echo "Note: Built with debug settings (default)."

# DEBUG:
debug: CXXFLAGS += $(DEBUG_FLAGS)
debug: common_rule
	@echo "Note: Built with debug settings."

# RELEASE:
release: CXXFLAGS += $(RELEASE_FLAGS)
release: common_rule
	@echo "Note: Built with release settings."

# Clang static check:
static_check: CXXFLAGS += $(DEBUG_FLAGS) $(STATIC_CHECK_FLAGS)
static_check: common_rule
	@echo "Note: Compiled for static analysis only. No code generated."

# Clang address sanitizer (ASan):
asan: CXXFLAGS += $(DEBUG_FLAGS) $(ASAN_FLAGS)
asan: common_rule
	@echo "Note: Built with address sanitizer enabled and debug settings."

#
# Base rules shared by all the above:
#

common_rule: $(CLI_BIN)
	$(QUIET) $(STRIP_CMD) $(CLI_BIN)

$(STATIC_LIB): $(OBJ_FILES)
	@echo "-> Creating static library ..."
	$(QUIET) $(AR_CMD) $@ $^

$(CLI_BIN): $(STATIC_LIB)
	@echo "-> Linking executable ..."
	$(QUIET) $(CXX) $(CXXFLAGS) $(CLI_SRC) -o $@ $(STATIC_LIB)

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
	$(QUIET) rm -f $(CLI_BIN) $(STATIC_LIB) $(LEX_GENERATED) $(BISON_GENERATED)
	$(QUIET) rm -f $(GENERATED_DIR)/*.hh $(GENERATED_DIR)/*.hpp
	$(QUIET) rm -rf $(OBJ_DIR) *.dSYM

