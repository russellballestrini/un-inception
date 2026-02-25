#!/usr/bin/env python3
# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

"""
Generate aggregated performance charts from multiple perf.json files.
Run via: build/un -a -f reports/4.2.0/perf.json -f reports/4.2.3/perf.json -f reports/4.2.4/perf.json scripts/generate-aggregated-charts.py
"""
import json
import os
from pathlib import Path
from statistics import mean
import matplotlib.pyplot as plt
import numpy as np

# Input files from /tmp/input/
input_dir = Path('/tmp/input')
output_dir = Path('/tmp/artifacts')
output_dir.mkdir(parents=True, exist_ok=True)

# Load all perf.json files
reports = {}
for json_file in sorted(input_dir.glob('*.json')):
    data = json.loads(json_file.read_text())
    tag = data.get('tag', json_file.stem)
    reports[tag] = data
    print(f"Loaded {tag}")

if not reports:
    print("ERROR: No JSON files found")
    exit(1)

print(f"\nAnalyzing {len(reports)} reports...")

# Extract data
versions = sorted(reports.keys())
avg_durations = [reports[v]['summary']['avg_duration_seconds'] for v in versions]

# Extract language timings
lang_timings = {}
for v in versions:
    lang_timings[v] = {}
    for lang_entry in reports[v]['languages']:
        lang = lang_entry['language']
        dur = lang_entry['duration_seconds']
        lang_timings[v][lang] = dur

# Calculate variance
all_langs = set()
for langs in lang_timings.values():
    all_langs.update(langs.keys())

lang_variance = {}
for lang in all_langs:
    durs = []
    for v in versions:
        if lang in lang_timings.get(v, {}):
            dur = lang_timings[v][lang]
            if isinstance(dur, (int, float)) and dur > 0:
                durs.append(dur)
    if len(durs) >= 2:
        pct = ((max(durs) - min(durs)) / min(durs) * 100) if min(durs) > 0 else 0
        lang_variance[lang] = {'min': min(durs), 'max': max(durs), 'pct': pct}

# Set dark theme
plt.style.use('dark_background')
plt.rcParams['figure.facecolor'] = '#1a1a2e'
plt.rcParams['axes.facecolor'] = '#16213e'

# Chart 1: Duration Trend
fig, ax = plt.subplots(figsize=(10, 6))
ax.plot(versions, avg_durations, marker='o', linewidth=2, markersize=10, color='#e94560')
ax.fill_between(range(len(versions)), avg_durations, alpha=0.3, color='#e94560')
ax.set_xlabel('Release', fontsize=12)
ax.set_ylabel('Average Duration (seconds)', fontsize=12)
ax.set_title('Average Test Duration Degradation Over Releases', fontsize=14, fontweight='bold')
ax.grid(True, alpha=0.3)
for i, (v, d) in enumerate(zip(versions, avg_durations)):
    ax.text(i, d + 1, f'{d}s', ha='center', fontsize=10)
plt.tight_layout()
plt.savefig(str(output_dir / 'aggregated-duration-trend.png'), dpi=150, facecolor='#1a1a2e')
print('✓ aggregated-duration-trend.png')
plt.close()

# Chart 2: Language Variance
top_langs = sorted(lang_variance.items(), key=lambda x: x[1]['pct'], reverse=True)[:15]
lang_names = [k.upper() for k, v in top_langs]
variances = [v['pct'] for k, v in top_langs]
colors = ['#e94560' if v > 200 else '#f39c12' if v > 100 else '#27ae60' for v in variances]

fig, ax = plt.subplots(figsize=(12, 8))
bars = ax.barh(lang_names, variances, color=colors, edgecolor='#fff', linewidth=1)
ax.set_xlabel('Variance %', fontsize=12)
ax.set_title('Top 15 Most Unstable Languages (% Variance)', fontsize=14, fontweight='bold')
for i, (bar, var) in enumerate(zip(bars, variances)):
    ax.text(var + 5, bar.get_y() + bar.get_height()/2, f'{var:.0f}%', va='center', fontsize=9)
plt.tight_layout()
plt.savefig(str(output_dir / 'aggregated-language-variance.png'), dpi=150, facecolor='#1a1a2e')
print('✓ aggregated-language-variance.png')
plt.close()

# Chart 3: Ranking Changes
fig, ax = plt.subplots(figsize=(14, 8))
for v in versions:
    sorted_langs = sorted(lang_timings[v].items(), key=lambda x: x[1], reverse=True)
    slowest_10 = sorted_langs[:10]
    for i, (lang, dur) in enumerate(slowest_10):
        ax.scatter(versions.index(v), i, s=300, alpha=0.6)

ax.set_xlabel('Release', fontsize=12)
ax.set_ylabel('Rank (0=Slowest)', fontsize=12)
ax.set_title('Ranking Instability - Top 10 Slowest Languages Per Run', fontsize=14, fontweight='bold')
ax.set_xticks(range(len(versions)))
ax.set_xticklabels(versions)
plt.tight_layout()
plt.savefig(str(output_dir / 'aggregated-ranking-changes.png'), dpi=150, facecolor='#1a1a2e')
print('✓ aggregated-ranking-changes.png')
plt.close()

print('\n✓ All charts generated successfully')
print(f'✓ Saved to {output_dir}/')
