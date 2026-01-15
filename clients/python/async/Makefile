.PHONY: help install dev-install test test-verbose test-coverage lint format clean docs examples

help:
	@echo "Unsandbox Async Python SDK - Available targets"
	@echo ""
	@echo "Installation:"
	@echo "  make install         Install package"
	@echo "  make dev-install     Install with dev dependencies"
	@echo ""
	@echo "Testing:"
	@echo "  make test            Run tests"
	@echo "  make test-verbose    Run tests with verbose output"
	@echo "  make test-coverage   Run tests with coverage report"
	@echo ""
	@echo "Code Quality:"
	@echo "  make lint            Lint code with flake8"
	@echo "  make format          Format code with black"
	@echo ""
	@echo "Other:"
	@echo "  make clean           Remove build artifacts"
	@echo "  make examples        Run examples"
	@echo "  make docs            Show README"

install:
	pip install -e .

dev-install:
	pip install -e ".[dev]"

test:
	pytest tests/ -q

test-verbose:
	pytest tests/ -v

test-coverage:
	pytest tests/ --cov=un_async --cov-report=html --cov-report=term

lint:
	flake8 src/ tests/ examples/
	mypy src/ --ignore-missing-imports

format:
	black src/ tests/ examples/

clean:
	find . -type d -name __pycache__ -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	rm -rf .pytest_cache
	rm -rf .mypy_cache
	rm -rf htmlcov
	rm -rf dist
	rm -rf build
	rm -rf *.egg-info

examples:
	@echo "Running examples..."
	@echo ""
	@echo "1. Hello World Example:"
	python examples/hello_world_async.py
	@echo ""
	@echo "2. Fibonacci Example:"
	python examples/fibonacci_async.py

docs:
	@cat README.md

.DEFAULT_GOAL := help
