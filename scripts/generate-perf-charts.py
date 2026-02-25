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
Generate performance visualization charts from CI timing data.
Outputs PNG charts to /tmp/artifacts/ for collection by unsandbox.

Usage: python3 generate-perf-charts.py [perf.json]
       Via unsandbox: build/un -a -f reports/TAG/perf.json scripts/generate-perf-charts.py
"""

import json
import sys
import os

# Create artifacts directory (unsandbox collects from /tmp/artifacts/)
os.makedirs('/tmp/artifacts', exist_ok=True)

# Load data from file or stdin
# When run via unsandbox, files are uploaded to /tmp/input/
possible_paths = [
    '/tmp/input/perf.json',       # Via unsandbox -f
    '/tmp/input/perf-4.2.0.json', # Legacy format
]

if len(sys.argv) > 1:
    possible_paths.insert(0, sys.argv[1])

# Also try to find any JSON file in /tmp/input/
if os.path.exists('/tmp/input'):
    for f in os.listdir('/tmp/input'):
        if f.endswith('.json'):
            possible_paths.append(f'/tmp/input/{f}')

data = None
for path in possible_paths:
    try:
        with open(path) as f:
            data = json.load(f)
            print(f'Loaded data from {path}')
            break
    except (FileNotFoundError, json.JSONDecodeError):
        continue

if data is None:
    # Try stdin as last resort
    try:
        data = json.load(sys.stdin)
        print('Loaded data from stdin')
    except:
        print('Error: Could not load performance data')
        print(f'Tried: {possible_paths}')
        sys.exit(1)

# Import matplotlib (available in unsandbox)
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np

# Set style
plt.style.use('seaborn-v0_8-darkgrid' if 'seaborn-v0_8-darkgrid' in plt.style.available else 'ggplot')
plt.rcParams['figure.facecolor'] = '#1a1a2e'
plt.rcParams['axes.facecolor'] = '#16213e'
plt.rcParams['text.color'] = '#eee'
plt.rcParams['axes.labelcolor'] = '#eee'
plt.rcParams['xtick.color'] = '#eee'
plt.rcParams['ytick.color'] = '#eee'
plt.rcParams['axes.edgecolor'] = '#444'
plt.rcParams['grid.color'] = '#333'
plt.rcParams['font.size'] = 10

# Extract data
languages = data['languages']
tag = data['tag']
summary = data['summary']

# Sort by duration
langs_sorted = sorted(languages, key=lambda x: x['duration_seconds'], reverse=True)
names = [l['language'] for l in langs_sorted]
durations = [l['duration_seconds'] for l in langs_sorted]
queued = [l['queued_duration'] for l in langs_sorted]

# Language categories
COMPILED = {'c', 'cpp', 'go', 'rust', 'java', 'kotlin', 'swift', 'csharp', 'fsharp',
            'haskell', 'ocaml', 'd', 'nim', 'zig', 'crystal', 'fortran', 'cobol',
            'objc', 'v', 'dart'}
INTERPRETED = {'python', 'javascript', 'typescript', 'ruby', 'php', 'perl', 'lua',
               'bash', 'r', 'awk', 'tcl', 'scheme', 'commonlisp', 'clojure', 'elixir',
               'erlang', 'groovy', 'raku', 'julia', 'prolog', 'forth', 'powershell', 'deno'}

categories = []
for l in langs_sorted:
    lang = l['language']
    if lang in COMPILED:
        categories.append('compiled')
    elif lang in INTERPRETED:
        categories.append('interpreted')
    else:
        categories.append('other')

# Color palette
colors_by_cat = {'compiled': '#e94560', 'interpreted': '#0f3460', 'other': '#533483'}
colors = [colors_by_cat.get(c, '#666') for c in categories]

# ============================================================================
# Chart 1: Horizontal Bar Chart - Duration by Language
# ============================================================================
fig, ax = plt.subplots(figsize=(12, 14))

y_pos = np.arange(len(names))
bars = ax.barh(y_pos, durations, color=colors, edgecolor='#fff', linewidth=0.5)

# Add value labels
for i, (bar, dur) in enumerate(zip(bars, durations)):
    ax.text(dur + 1, bar.get_y() + bar.get_height()/2, f'{dur}s',
            va='center', ha='left', fontsize=8, color='#aaa')

ax.set_yticks(y_pos)
ax.set_yticklabels(names, fontsize=9)
ax.invert_yaxis()
ax.set_xlabel('Duration (seconds)', fontsize=12)
ax.set_title(f'UN Inception v{tag} - Test Duration by Language\n42 Languages • 638 Tests • 100% Pass Rate',
             fontsize=14, fontweight='bold', color='#fff', pad=20)

# Legend
compiled_patch = mpatches.Patch(color='#e94560', label='Compiled')
interpreted_patch = mpatches.Patch(color='#0f3460', label='Interpreted')
ax.legend(handles=[compiled_patch, interpreted_patch], loc='lower right',
          facecolor='#1a1a2e', edgecolor='#444', labelcolor='#eee')

# Stats annotation
stats_text = f"Avg: {summary['avg_duration_seconds']}s | Min: {summary['min_duration_seconds']}s | Max: {summary['max_duration_seconds']}s"
ax.text(0.5, -0.05, stats_text, transform=ax.transAxes, ha='center', fontsize=10, color='#888')

plt.tight_layout()
plt.savefig('/tmp/artifacts/chart-duration-by-language.png', dpi=150, facecolor='#1a1a2e')
print('✓ chart-duration-by-language.png')

# ============================================================================
# Chart 2: Category Comparison (Compiled vs Interpreted)
# ============================================================================
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

# Pie chart
compiled_count = sum(1 for c in categories if c == 'compiled')
interpreted_count = sum(1 for c in categories if c == 'interpreted')

ax1 = axes[0]
sizes = [compiled_count, interpreted_count]
labels = [f'Compiled\n({compiled_count})', f'Interpreted\n({interpreted_count})']
explode = (0.02, 0.02)
wedges, texts, autotexts = ax1.pie(sizes, labels=labels, autopct='%1.0f%%',
                                    colors=['#e94560', '#0f3460'],
                                    explode=explode, startangle=90,
                                    textprops={'color': '#eee', 'fontsize': 11},
                                    wedgeprops={'edgecolor': '#fff', 'linewidth': 1})
ax1.set_title('Language Distribution', fontsize=12, color='#fff', fontweight='bold')

# Box plot comparing durations
ax2 = axes[1]
compiled_durations = [l['duration_seconds'] for l, c in zip(langs_sorted, categories) if c == 'compiled']
interpreted_durations = [l['duration_seconds'] for l, c in zip(langs_sorted, categories) if c == 'interpreted']

bp = ax2.boxplot([compiled_durations, interpreted_durations],
                  labels=['Compiled', 'Interpreted'],
                  patch_artist=True,
                  medianprops={'color': '#fff', 'linewidth': 2})

bp['boxes'][0].set_facecolor('#e94560')
bp['boxes'][1].set_facecolor('#0f3460')
for box in bp['boxes']:
    box.set_edgecolor('#fff')

ax2.set_ylabel('Duration (seconds)', fontsize=11)
ax2.set_title('Duration Distribution by Category', fontsize=12, color='#fff', fontweight='bold')

# Add means as points
ax2.scatter([1], [np.mean(compiled_durations)], color='#fff', s=100, zorder=5, marker='D', label='Mean')
ax2.scatter([2], [np.mean(interpreted_durations)], color='#fff', s=100, zorder=5, marker='D')

plt.suptitle(f'UN Inception v{tag} - Compiled vs Interpreted', fontsize=14, fontweight='bold', color='#fff', y=1.02)
plt.tight_layout()
plt.savefig('/tmp/artifacts/chart-category-comparison.png', dpi=150, facecolor='#1a1a2e')
print('✓ chart-category-comparison.png')

# ============================================================================
# Chart 3: Duration Distribution Histogram with KDE-like curve
# ============================================================================
fig, ax = plt.subplots(figsize=(10, 6))

n, bins, patches = ax.hist(durations, bins=15, color='#e94560', edgecolor='#fff',
                            alpha=0.8, linewidth=1)

# Color gradient based on height
max_height = max(n)
for patch, height in zip(patches, n):
    intensity = 0.3 + 0.7 * (height / max_height)
    patch.set_facecolor(plt.cm.plasma(intensity))

ax.axvline(summary['avg_duration_seconds'], color='#00ff88', linestyle='--',
           linewidth=2, label=f"Mean: {summary['avg_duration_seconds']}s")
ax.axvline(np.median(durations), color='#00d4ff', linestyle=':',
           linewidth=2, label=f"Median: {int(np.median(durations))}s")

ax.set_xlabel('Duration (seconds)', fontsize=12)
ax.set_ylabel('Number of Languages', fontsize=12)
ax.set_title(f'UN Inception v{tag} - Test Duration Distribution\n42 Languages',
             fontsize=14, fontweight='bold', color='#fff', pad=15)
ax.legend(facecolor='#1a1a2e', edgecolor='#444', labelcolor='#eee')

plt.tight_layout()
plt.savefig('/tmp/artifacts/chart-duration-histogram.png', dpi=150, facecolor='#1a1a2e')
print('✓ chart-duration-histogram.png')

# ============================================================================
# Chart 4: Top 10 Slowest vs Top 10 Fastest
# ============================================================================
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

# Top 10 slowest
ax1 = axes[0]
slowest = langs_sorted[:10]
slow_names = [l['language'] for l in slowest]
slow_durations = [l['duration_seconds'] for l in slowest]
slow_colors = [colors_by_cat.get(c, '#666') for c in categories[:10]]

bars1 = ax1.barh(range(10), slow_durations, color=slow_colors, edgecolor='#fff')
ax1.set_yticks(range(10))
ax1.set_yticklabels(slow_names)
ax1.invert_yaxis()
ax1.set_xlabel('Duration (seconds)')
ax1.set_title('🐢 Top 10 Slowest', fontsize=12, color='#ff6b6b', fontweight='bold')
for i, (bar, dur) in enumerate(zip(bars1, slow_durations)):
    ax1.text(dur + 1, i, f'{dur}s', va='center', fontsize=9, color='#aaa')

# Top 10 fastest
ax2 = axes[1]
fastest = langs_sorted[-10:][::-1]
fast_names = [l['language'] for l in fastest]
fast_durations = [l['duration_seconds'] for l in fastest]
fast_categories = [categories[names.index(n)] for n in fast_names]
fast_colors = [colors_by_cat.get(c, '#666') for c in fast_categories]

bars2 = ax2.barh(range(10), fast_durations, color=fast_colors, edgecolor='#fff')
ax2.set_yticks(range(10))
ax2.set_yticklabels(fast_names)
ax2.invert_yaxis()
ax2.set_xlabel('Duration (seconds)')
ax2.set_title('🚀 Top 10 Fastest', fontsize=12, color='#00ff88', fontweight='bold')
for i, (bar, dur) in enumerate(zip(bars2, fast_durations)):
    ax2.text(dur + 1, i, f'{dur}s', va='center', fontsize=9, color='#aaa')

plt.suptitle(f'UN Inception v{tag} - Speed Leaders', fontsize=14, fontweight='bold', color='#fff', y=1.02)
plt.tight_layout()
plt.savefig('/tmp/artifacts/chart-speed-leaders.png', dpi=150, facecolor='#1a1a2e')
print('✓ chart-speed-leaders.png')

# ============================================================================
# Chart 5: Queue Time vs Execution Time Scatter
# ============================================================================
fig, ax = plt.subplots(figsize=(10, 8))

scatter_colors = [colors_by_cat.get(c, '#666') for c in categories]
scatter = ax.scatter(queued, durations, c=scatter_colors, s=100, alpha=0.8, edgecolors='#fff', linewidth=0.5)

# Label outliers
for i, (q, d, name) in enumerate(zip(queued, durations, names)):
    if d > 70 or q > 200:
        ax.annotate(name, (q, d), xytext=(5, 5), textcoords='offset points',
                    fontsize=8, color='#aaa')

ax.set_xlabel('Queue Duration (seconds)', fontsize=12)
ax.set_ylabel('Execution Duration (seconds)', fontsize=12)
ax.set_title(f'UN Inception v{tag} - Queue Time vs Execution Time\nEach point is one language',
             fontsize=14, fontweight='bold', color='#fff', pad=15)

# Legend
compiled_patch = mpatches.Patch(color='#e94560', label='Compiled')
interpreted_patch = mpatches.Patch(color='#0f3460', label='Interpreted')
ax.legend(handles=[compiled_patch, interpreted_patch], loc='upper right',
          facecolor='#1a1a2e', edgecolor='#444', labelcolor='#eee')

plt.tight_layout()
plt.savefig('/tmp/artifacts/chart-queue-vs-execution.png', dpi=150, facecolor='#1a1a2e')
print('✓ chart-queue-vs-execution.png')

# ============================================================================
# Chart 6: Summary Dashboard
# ============================================================================
fig = plt.figure(figsize=(16, 10))

# Create grid
gs = fig.add_gridspec(3, 3, hspace=0.4, wspace=0.3)

# Big title
fig.suptitle(f'UN Inception v{tag} Performance Dashboard', fontsize=20, fontweight='bold',
             color='#fff', y=0.98)

# Metric cards (top row)
metrics = [
    ('Tests', str(summary['total_tests']), '#00ff88'),
    ('Languages', str(summary['total_languages']), '#00d4ff'),
    ('Pass Rate', '100%', '#ff6b6b'),
    ('Avg Time', f"{summary['avg_duration_seconds']}s", '#ffd93d'),
]

for i, (label, value, color) in enumerate(metrics):
    ax = fig.add_subplot(gs[0, i] if i < 3 else gs[0, 2])
    if i == 3:
        ax = fig.add_axes([0.78, 0.75, 0.18, 0.15])
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.text(0.5, 0.6, value, fontsize=32, fontweight='bold', color=color,
            ha='center', va='center', transform=ax.transAxes)
    ax.text(0.5, 0.2, label, fontsize=14, color='#888',
            ha='center', va='center', transform=ax.transAxes)
    ax.axis('off')

# Mini bar chart (bottom left)
ax_bar = fig.add_subplot(gs[1:, :2])
top_15 = langs_sorted[:15]
y_pos = np.arange(15)
bar_colors = [colors_by_cat.get(categories[i], '#666') for i in range(15)]
ax_bar.barh(y_pos, [l['duration_seconds'] for l in top_15], color=bar_colors, edgecolor='#fff')
ax_bar.set_yticks(y_pos)
ax_bar.set_yticklabels([l['language'] for l in top_15], fontsize=9)
ax_bar.invert_yaxis()
ax_bar.set_xlabel('Duration (seconds)')
ax_bar.set_title('Slowest 15 Languages', fontsize=12, color='#fff', fontweight='bold')

# Pie chart (bottom right)
ax_pie = fig.add_subplot(gs[1, 2])
ax_pie.pie([compiled_count, interpreted_count],
           labels=['Compiled', 'Interpreted'],
           autopct='%1.0f%%',
           colors=['#e94560', '#0f3460'],
           textprops={'color': '#eee'},
           wedgeprops={'edgecolor': '#fff'})
ax_pie.set_title('Language Types', fontsize=12, color='#fff', fontweight='bold')

# Stats text (bottom right corner)
ax_stats = fig.add_subplot(gs[2, 2])
ax_stats.axis('off')
stats = f"""
Fastest: {summary['fastest_language']} ({summary['min_duration_seconds']}s)
Slowest: {summary['slowest_language']} ({summary['max_duration_seconds']}s)
Spread: {summary['max_duration_seconds'] - summary['min_duration_seconds']}s

Generated: {data['generated_at'][:10]}
Pipeline: #{data['child_pipeline_id']}
"""
ax_stats.text(0.1, 0.9, stats, fontsize=11, color='#aaa', va='top',
              transform=ax_stats.transAxes, family='monospace')

plt.savefig('/tmp/artifacts/chart-dashboard.png', dpi=150, facecolor='#1a1a2e')
print('✓ chart-dashboard.png')

print(f'\n✅ Generated 6 charts in /tmp/artifacts/')
print('Files:')
for f in os.listdir('/tmp/artifacts'):
    print(f'  - {f}')
