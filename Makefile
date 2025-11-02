
# Define commands ----
RSCRIPT := Rscript


# C++ compiler ----
CC = /opt/homebrew/bin/gcc-15
CXX = /opt/homebrew/bin/g++-15
CXXFLAGS = -O3 -Wno-psabi -std=c++20 -march=native \
  -I/Users/pietrodonelli/Documents/University/fdaPDE/fdaPDE-cpp \
  -I/Users/pietrodonelli/Documents/University/fdaPDE/fdaPDE-cpp/fdaPDE/core \
  -I/opt/homebrew/include/eigen3 \


# Targets ----
.PHONY: help install install_femR build  \
        complile compile_all \
        clean_options clean_compiled clean  distclean \
        run_test run_test_parallel inspect_results


# Default target ----
all: install build


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
build: install compile_all
	@echo "Creating necessary directories..."
	@mkdir -p results
	@mkdir -p images
	@echo "\nBuild completed.\n"
	
## Compile C++ model ----

# Discover models under cpp/, excluding 'include'
MODELS := $(filter-out include,$(notdir $(wildcard cpp/*)))
MODELS := $(filter-out $(filter-out %/,$(patsubst %/,%,$(foreach d,$(MODELS),$(if $(wildcard cpp/$(d)/.),$(d),)))), $(MODELS))

## Compile all models under cpp/
compile_all:
	@echo "\nCompiling all models in cpp/..."
	@for model in $$(find cpp -mindepth 1 -maxdepth 1 -type d ! -name include -exec basename {} \; | sort); do \
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
		find cpp -mindepth 1 -maxdepth 1 -type d ! -name include -exec basename {} \; | \
		while read m; do \
			ls "cpp/$$m"/main*.cpp >/dev/null 2>&1 && echo $$m; \
		done | sort | sed 's/^\(.*\)/- \1 (make compile MODEL=\1)/'; \
		echo ""; \
		exit 0; \
	elif [ ! -d "cpp/$(MODEL)" ]; then \
		echo "\nError: model directory cpp/$(MODEL) not found."; \
		exit 1; \
	else \
		echo "\nCompiling mains in cpp/$(MODEL) ..."; \
		mains=$$(ls cpp/$(MODEL)/main*.cpp 2>/dev/null || true); \
		if [ -z "$$mains" ]; then \
			echo "No main*.cpp found in cpp/$(MODEL)"; \
			exit 1; \
		fi; \
		for src in $$mains; do \
			base=$$(basename "$$src"); \
			case "$$base" in \
				main.cpp) bin="fit_model" ;; \
				main_*.cpp) stem=$${base#main_}; stem=$${stem%.cpp}; bin="fit_model_$$stem" ;; \
				*) continue ;; \
			esac; \
			out="cpp/$(MODEL)/$$bin"; \
			if [ ! -f "$$out" ] || [ "$$src" -nt "$$out" ]; then \
				echo "- $$src  ==>  $$out"; \
				$(CXX) -o "$$out" "$$src" $(CXXFLAGS); \
			else \
				echo "- $$out is up to date"; \
			fi; \
		done; \
		echo "All the source files have been compiled!\n"; \
	fi

# Clean targets ----

## Clean temporary files
clean_tmp:
	@$(RM) -r tmp/
	
## Clean compiled binaries
clean_compiled:
	@$(RM) cpp/*/fit_model cpp/*/fit_model_*

## Clean temporary files, logs and R session files
clean: clean_tmp
	@echo "\nCleaning temporary files..."
	@$(RM) *.aux *.log *.pdf *.txt *.json
	@$(RM) .Rhistory
	@$(RM) .RData
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
		$(RM) -r results/$(TEST_SUITE)/$(TEST_NAME); \
		$(RM) -r images/$(TEST_SUITE)/$(TEST_NAME); \
		$(RM) -r data/tests/$(TEST_SUITE)/$(TEST_NAME); \
		echo "Cleanup completed for test: $(TEST_NAME)"; \
	fi
	
## DANGER ZONE: Full cleanup of all generated files
distclean: clean clean_compiled
	@echo "Attention! This will remove ALL the additional files and directories generated so far."
	@read -p "Are you sure you want to continue? [y/n]: " confirm && [ "$$confirm" = "y" ] || (echo "Cleanup aborted." && false)
	@echo "Removing additional generated files..."
	@$(RM) -r images/
	@$(RM) -r results/
	@$(RM) -r data/tests/
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
		./run_tests.sh "$(TEST_SUITE)" "$(TEST_NAME)"; \
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
		./run_tests_parallel.sh "$(TEST_SUITE)" "$(TEST_NAME)"; \
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
		queue_directory="tmp/queue/$(TEST_SUITE)/$(TEST_NAME)"; \
		RScript src/init.R $(TEST_SUITE) $(TEST_NAME) \
		echo "Available files in $$queue_directory:"; \
		files=($$(ls -1 $$queue_directory 2>/dev/null)); \
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
			Rscript "src/init.R" "$(TEST_SUITE)" "$(TEST_NAME)"; \
			Rscript "tests/$(TEST_SUITE)/inspect_results.R" "$(TEST_NAME)" "$$selected"; \
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