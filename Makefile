
#######################################

# Define 'VERBOSE' to get the full console output.
# Otherwise print a short message for each rule.
ifndef VERBOSE
  CXX            = @echo "-> Compiling "$<" ..." && c++
  BISON          = @bison
  LEX            = @flex
  RM             = @rm
else
  CXX            = c++
  BISON          = bison
  LEX            = flex
  RM             = rm
endif

BIN_TARGET       = moon
GENERATED_DIR    = generated
SOURCE_DIR       = source
LEX_SRC          = $(wildcard $(SOURCE_DIR)/*.lxx)
BISON_SRC        = $(wildcard $(SOURCE_DIR)/*.yxx)
CPP_SRC          = $(wildcard $(SOURCE_DIR)/*.cpp)
LEX_GENERATED    = $(addprefix $(GENERATED_DIR)/, $(notdir $(patsubst %.lxx, %.cpp, $(LEX_SRC))))
BISON_GENERATED  = $(addprefix $(GENERATED_DIR)/, $(notdir $(patsubst %.yxx, %.cpp, $(BISON_SRC))))
SRC              = $(BISON_GENERATED) $(LEX_GENERATED) $(CPP_SRC)
OBJ              = $(patsubst %.cpp, %.o, $(SRC))

DEBUG_FLAGS = -g                \
              -DDEBUG=1         \
              -D_DEBUG=1        \
              -D_LIBCPP_DEBUG=0 \
              -D_LIBCPP_DEBUG2=0

GLOBAL_DEFINES = -DMOON_DEBUG_MEMORY=1  \
                 -DMOON_ENABLE_ASSERT=1 \
                 -DMOON_PRINT_USE_ANSI_COLOR_CODES=1

WARNS_USED = -Wall                   \
             -Wextra                 \
             -Weffc++                \
             -pedantic               \
             -Wsequence-point        \
             -Wdisabled-optimization \
             -Wuninitialized         \
             -Wshadow                \
             -Wformat=2              \
             -Winit-self

WARNS_IGNORED = -Wno-deprecated-register \
                -Wno-unused-function     \
                -Wno-sign-compare

CXXFLAGS = -std=c++11           \
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

#######################################

all: $(OBJ)
	$(CXX) $(CXXFLAGS) -o $(BIN_TARGET) $(OBJ)

$(BIN_TARGET): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $* $(OBJ)

#######################################

# Preserves the generated source files
.PRECIOUS: $(GENERATED_DIR)/%.cpp

$(GENERATED_DIR)/%.cpp: $(SOURCE_DIR)/%.yxx
	@echo "-> Running Bison for "$<" ..."
	$(BISON) -o $@ -d $<

#######################################

# Preserves the generated source files
.PRECIOUS: $(GENERATED_DIR)/%.cpp

$(GENERATED_DIR)/%.cpp: $(SOURCE_DIR)/%.lxx
	@echo "-> Running Flex for "$<" ..."
	$(LEX) -t $< > $@

#######################################

clean:
	@echo "-> Cleaning ..."
	$(RM) -f $(OBJ) $(BIN_TARGET) $(LEX_GENERATED) $(BISON_GENERATED)
	$(RM) -f $(GENERATED_DIR)/*.hh $(GENERATED_DIR)/*.hpp

