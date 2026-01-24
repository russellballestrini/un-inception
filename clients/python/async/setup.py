#!/usr/bin/env python3
"""
Setup script for unsandbox async Python SDK
"""

from setuptools import setup, find_packages

setup(
    name="unsandbox-async",
    version="4.2.24",
    description="Asynchronous Python SDK for unsandbox.com code execution",
    long_description=open("README.md").read() if False else "Async Python SDK for unsandbox code execution",
    author="unsandbox.com",
    url="https://github.com/unsandbox/un-inception",
    license="Public Domain",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    python_requires=">=3.7",
    install_requires=[
        "aiohttp>=3.8.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.0",
            "pytest-asyncio>=0.20.0",
            "black>=22.0",
            "flake8>=4.0",
            "mypy>=0.950",
        ],
    },
    entry_points={},
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: Public Domain License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Internet :: WWW/HTTP",
    ],
)
