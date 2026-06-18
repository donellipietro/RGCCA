
# Define commands ----
SHELL := /bin/bash
RSCRIPT ?= Rscript
TESTBENCH_PROFILE ?= macbook

define config_value
$(strip $(shell $(RSCRIPT) -e 'source("config.R"); cfg <- get_config("$(TESTBENCH_PROFILE)"); value <- cfg[["$(1)"]]; if (is.null(value)) value <- ""; cat(value)'))
endef


# C++ compiler ----
PATH_REPO := $(call config_value,PATH_REPO)
PATH_CPP := $(call config_value,PATH_CPP)
PATH_RESULTS := $(call config_value,PATH_RESULTS)
PATH_IMAGES := $(call config_value,PATH_IMAGES)
PATH_TEST_DATA := $(call config_value,PATH_TEST_DATA)
PATH_TMP := $(call config_value,PATH_TMP)
PATH_QUEUE := $(call config_value,PATH_QUEUE)
PATH_LOGS := $(call config_value,PATH_LOGS)
PATH_TMP_DATA := $(call config_value,PATH_TMP_DATA)
PATH_TMP_RESULTS := $(call config_value,PATH_TMP_RESULTS)
PATH_BUILD := $(call config_value,PATH_BUILD)

CC = $(call config_value,CC)
CXX = $(call config_value,CXX)
PATH_FDAPDE_CPP := $(call config_value,PATH_FDAPDE_CPP)
PATH_FDAPDE_CORE := $(call config_value,PATH_FDAPDE_CORE)
PATH_IPOPT_INCLUDE := $(call config_value,PATH_IPOPT_INCLUDE)
PATH_IPOPT_LIB := $(call config_value,PATH_IPOPT_LIB)
PATH_EIGEN_INCLUDE := $(call config_value,PATH_EIGEN_INCLUDE)
SINGULARITY_IMAGE := $(call config_value,SINGULARITY_IMAGE)
SINGULARITY_BIND_PATHS := $(call config_value,SINGULARITY_BIND_PATHS)

INCLUDE_DIRS := $(PATH_FDAPDE_CPP) $(PATH_FDAPDE_CORE) $(PATH_IPOPT_INCLUDE) $(PATH_EIGEN_INCLUDE)
CXX_INCLUDES := $(foreach dir,$(INCLUDE_DIRS),$(if $(strip $(dir)),-I$(dir),))
LIB_DIRS := $(PATH_IPOPT_LIB)
LIBRARY_PATHS := $(foreach dir,$(LIB_DIRS),$(if $(strip $(dir)),-L$(dir),))

CXXFLAGS ?= -O3 -Wno-psabi -std=c++20 -march=native -DFDAPDE_ENABLE_COUT $(CXX_INCLUDES)
LDFLAGS ?= $(LIBRARY_PATHS)
LDLIBS ?= -lipopt

ifeq ($(strip $(SINGULARITY_IMAGE)),)
RUNTIME_PREFIX :=
else
RUNTIME_PREFIX := singularity exec $(if $(strip $(SINGULARITY_BIND_PATHS)),--bind "$(SINGULARITY_BIND_PATHS)") "$(SINGULARITY_IMAGE)"
endif



# Targets ----
.PHONY: help config write_env install install_femR build  \
        complile compile_all \
        clean_options clean_compiled clean  distclean \
        run_test run_test_parallel inspect_results


# Default target ----
all: install build


# Config targets ----
config:
	@$(RSCRIPT) config.R --profile "$(TESTBENCH_PROFILE)" --print

write_env:
	@$(RSCRIPT) config.R --profile "$(TESTBENCH_PROFILE)" --write-env


# Installation targets ----
install_femR:
	@echo "\nInstalling femR..."
	@$(RSCRIPT) src/installation/install_femR.R
# install_fdaPDE:
# 	@echo "\nInstalling fdaPDE..."
# 	@$(RSCRIPT) src/installation/install_fdaPDE.R
install:  install_femR 
	@echo "\nInstallation completed."


# Build target ----  
# compile_all
build: install
	@echo "Creating necessary directories..."
	@mkdir -p "$(PATH_RESULTS)" "$(PATH_IMAGES)" "$(PATH_TEST_DATA)"
	@mkdir -p "$(PATH_TMP)" "$(PATH_QUEUE)" "$(PATH_LOGS)" "$(PATH_TMP_DATA)" "$(PATH_TMP_RESULTS)" "$(PATH_BUILD)"
	@echo "\nBuild completed.\n"
	
## Compile C++ model ----

# Discover models under cpp/, excluding 'include'
MODELS := $(filter-out include,$(notdir $(wildcard $(PATH_CPP)/*)))

## Compile all models under cpp/
compile_all:
	@echo "\nCompiling all models in $(PATH_CPP)..."
	@for model in $$(find "$(PATH_CPP)" -mindepth 1 -maxdepth 1 -type d ! -name include -exec basename {} \; | sort); do \
		$(MAKE) --no-print-directory compile MODEL=$$model || exit $$?; \
	done
	@echo "All models compiled successfully.\n"

## Compile all mains found in cpp/$(MODEL)
# Usage: make compile MODEL=my_model
compile:
	@if [ -z "$(MODEL)" ]; then \
		echo "\nUsage: make compile MODEL=<model_name>"; \
		echo ""; \
		echo "Available MODELS:"; \
		find "$(PATH_CPP)" -mindepth 1 -maxdepth 1 -type d ! -name include -exec basename {} \; | \
		while read m; do \
			ls "$(PATH_CPP)/$$m"/main*.cpp >/dev/null 2>&1 && echo $$m; \
		done | sort | sed 's/^\(.*\)/- \1 (make compile MODEL=\1)/'; \
		echo ""; \
		exit 0; \
	elif [ ! -d "$(PATH_CPP)/$(MODEL)" ]; then \
		echo "\nError: model directory $(PATH_CPP)/$(MODEL) not found."; \
		exit 1; \
	else \
		echo "\nCompiling mains in cpp/$(MODEL) ..."; \
		mains=$$(ls "$(PATH_CPP)/$(MODEL)"/main*.cpp 2>/dev/null || true); \
		if [ -z "$$mains" ]; then \
			echo "No main*.cpp found in $(PATH_CPP)/$(MODEL)"; \
			exit 1; \
		fi; \
		for src in $$mains; do \
			base=$$(basename "$$src"); \
			case "$$base" in \
				main.cpp) bin="fit_model" ;; \
				main_*.cpp) stem=$${base#main_}; stem=$${stem%.cpp}; bin="fit_model_$$stem" ;; \
				*) continue ;; \
			esac; \
			out="$(PATH_CPP)/$(MODEL)/$$bin"; \
			if [ ! -f "$$out" ] || [ "$$src" -nt "$$out" ]; then \
				echo "- $$src  ==>  $$out"; \
				$(RUNTIME_PREFIX) $(CXX) -o "$$out" "$$src" $(CXXFLAGS) $(LDFLAGS) $(LDLIBS); \
			else \
				echo "- $$out is up to date"; \
			fi; \
		done; \
		echo "All the source files have been compiled!\n"; \
	fi

# Clean targets ----

## Clean temporary files
clean_tmp:
	@$(RM) -r "$(PATH_TMP)"
	
## Clean compiled binaries
clean_compiled:
	@find "$(PATH_CPP)" -mindepth 2 -maxdepth 2 -type f \( -name 'fit_model' -o -name 'fit_model_*' \) -exec $(RM) {} +

## Clean temporary files, logs and R session files
clean: clean_tmp
	@echo "\nCleaning temporary files..."
	@$(RM) *.aux *.log *.pdf *.txt *.json
	@$(RM) .Rhistory
	@$(RM) .RData
	@$(RM) .env
	@echo "Cleanup completed.\n"
	
## Clean results and images of a specific test
# - usage: make clean_test TEST_SUITE=centering TEST_NAME=test1
clean_test:
	@if [ -z "$(TEST_SUITE)" ] || [ -z "$(TEST_NAME)" ]; then \
		echo "Usage: make clean_test TEST_SUITE=<suite> TEST_NAME=<test_name>"; \
		echo ""; \
		echo "Available TEST_SUITEs:"; \
		find tests -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort | \
		sed 's/^\(.*\)/- \1 (make clean_test TEST_SUITE=\1 TEST_NAME=<test_name>)/'; \
		echo ""; \
		exit 0; \
	else \
		echo "Cleaning results and images for test: $(TEST_NAME) from suite: $(TEST_SUITE)"; \
		$(RM) -r "$(PATH_RESULTS)/$(TEST_SUITE)/$(TEST_NAME)"; \
		$(RM) -r "$(PATH_IMAGES)/$(TEST_SUITE)/$(TEST_NAME)"; \
		$(RM) -r "$(PATH_TEST_DATA)/$(TEST_SUITE)/$(TEST_NAME)"; \
		echo "Cleanup completed for test: $(TEST_NAME)"; \
	fi
	
## DANGER ZONE: Full cleanup of all generated files
distclean: clean clean_compiled
	@echo "Attention! This will remove ALL the additional files and directories generated so far."
	@read -p "Are you sure you want to continue? [y/n]: " confirm && [ "$$confirm" = "y" ] || (echo "Cleanup aborted." && false)
	@echo "Removing additional generated files..."
	@$(RM) -r "$(PATH_IMAGES)"
	@$(RM) -r "$(PATH_RESULTS)"
	@$(RM) -r "$(PATH_TEST_DATA)"
	@echo "Additional cleanup completed.\n"

# Test targets ----

## Run all the batches of a test sequentially
# usage: make run_test TEST_SUITE=centering TEST_NAME=test1
run_test: build
	@if [ -z "$(TEST_SUITE)" ] || [ -z "$(TEST_NAME)" ]; then \
		echo "Usage: make run_test TEST_SUITE=<suite> TEST_NAME=<test_name>"; \
		echo ""; \
		echo "Available TEST_SUITEs:"; \
		find tests -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort | \
		sed 's/^\(.*\)/- \1 (make run_test TEST_SUITE=\1 TEST_NAME=<test_name>)/'; \
		echo ""; \
		exit 0; \
	else \
		echo "Running: $(TEST_NAME) from suite $(TEST_SUITE)"; \
		TESTBENCH_PROFILE="$(TESTBENCH_PROFILE)" ./run_tests.sh "$(TEST_SUITE)" "$(TEST_NAME)"; \
	fi
	
## Run all the batches of a test in parallel
# usage: make run_test_parallel TEST_SUITE=centering TEST_NAME=test1
run_test_parallel: build
	@if [ -z "$(TEST_SUITE)" ] || [ -z "$(TEST_NAME)" ]; then \
		echo "Usage: make run_test_parallel TEST_SUITE=<suite> TEST_NAME=<test_name>"; \
		echo ""; \
		echo "Available TEST_SUITEs:"; \
		find tests -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort | \
		sed 's/^\(.*\)/- \1 (make run_test_parallel TEST_SUITE=\1 TEST_NAME=<test_name>)/'; \
		echo ""; \
		exit 0; \
	else \
		echo "Running: $(TEST_NAME) from suite $(TEST_SUITE)"; \
		TESTBENCH_PROFILE="$(TESTBENCH_PROFILE)" ./run_tests_parallel.sh "$(TEST_SUITE)" "$(TEST_NAME)"; \
	fi

## Inspect results of a specific test interactively
# Usage: make inspect_results TEST_SUITE=<suite> TEST_NAME=<test_name>
# Lists available result files in tmp/queue/<suite>/<test>, lets you select one,
# and runs the corresponding R scripts to visualize or analyze it.
inspect_results:
	@if [ -z "$(TEST_SUITE)" ] || [ -z "$(TEST_NAME)" ]; then \
		echo "\nUsage: make inspect_results TEST_SUITE=<suite> TEST_NAME=<test_name>"; \
		echo ""; \
		echo "Available TEST_SUITEs:"; \
		find tests -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort | \
		sed 's/^\(.*\)/- \1 (make inspect_results TEST_SUITE=\1 TEST_NAME=<test_name>)/'; \
		echo ""; \
		exit 0; \
	else \
		queue_directory="$(PATH_QUEUE)/$(TEST_SUITE)/$(TEST_NAME)"; \
		TESTBENCH_PROFILE="$(TESTBENCH_PROFILE)" $(RSCRIPT) src/init.R "$(TEST_SUITE)" "$(TEST_NAME)"; \
		echo "Available files in $$queue_directory:"; \
		files=($$(ls -1 "$$queue_directory" 2>/dev/null)); \
		if [ $${#files[@]} -eq 0 ]; then \
			echo "No files found in $$queue_directory."; \
			exit 1; \
		fi; \
		count=$${#files[@]}; \
		for i in $$(seq 1 $$count); do \
			echo "  $$i) $${files[$$((i-1))]}"; \
		done; \
		echo ""; \
		read -p "Select a file number: " choice; \
		if [ $$choice -ge 1 ] && [ $$choice -le $$count ]; then \
			selected=$${files[$$((choice-1))]}; \
			echo "Running RScript with selected file: $$selected"; \
			TESTBENCH_PROFILE="$(TESTBENCH_PROFILE)" $(RSCRIPT) "src/init.R" "$(TEST_SUITE)" "$(TEST_NAME)"; \
			TESTBENCH_PROFILE="$(TESTBENCH_PROFILE)" $(RSCRIPT) "tests/$(TEST_SUITE)/inspect_results.R" "$(TEST_NAME)" "$$selected"; \
		else \
			echo "Invalid choice!"; \
			exit 1; \
		fi; \
	fi

	
	
	
## Show available targets and descriptions
# Pretty printing for help (tweak width/color as you like)
HELP_FMT ?= \033[36m- %-24s\033[0m %s\n
help:
	@echo "\nAvailable targets:"
	@awk -v fmt="$(HELP_FMT)" '\
/^[a-zA-Z0-9_.-]+:.*##/ { \
  line=$$0; \
  tgt=line; sub(/:.*/,"",tgt); \
  desc=line; sub(/.*##[[:space:]]*/,"",desc); \
  printf fmt, tgt, desc; \
  next \
} \
/^##/ { \
  line=$$0; sub(/^##[[:space:]]*/,"",line); \
  if (desc) desc = desc " " line; else desc = line; \
  next \
} \
/^[a-zA-Z0-9_.-]+:/ { \
  if (desc) { \
    tgt=$$0; sub(/:.*/,"",tgt); \
    printf fmt, tgt, desc; \
    desc=""; \
  } \
  next \
} \
' $(MAKEFILE_LIST)
	@echo ""
