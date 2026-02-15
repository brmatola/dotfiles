---
title: Emacs Graph View
status: not_started
tags: [emacs, trellis, graph]
depends_on:
  - active/graph-headless
description: Embed trellis DAG viewer in Emacs via xwidget-webkit with managed server lifecycle
---

# Emacs Graph View

## Problem

The trellis graph viewer is a browser-based DAG visualization. Viewing it requires switching from Emacs to a browser, breaking flow. We want to see it from within a workspace without leaving Emacs.

## Solution

A new `claude-graph.el` module that manages a `trellis graph` server process and displays the viewer in an xwidget-webkit buffer. The server starts on demand, and dies when the buffer is dismissed.

## Prerequisites

- Emacs compiled with xwidget-webkit support (`--with-xwidgets`)
- `trellis graph --no-open` flag (from upstream `graph-headless` plan)

## Design

### Server Lifecycle

```
User hits SPC = g (from dashboard)
  │
  ├─ Server already running + buffer exists? → switch to buffer
  ├─ Server already running + no buffer?     → create xwidget buffer at URL
  └─ No server?
       ├─ Start `trellis graph --no-open --port 19847` as async process
       ├─ Watch stdout for "Serving DAG viewer at ..."
       └─ On match → open xwidget-webkit buffer at URL
                    → register kill-buffer-hook to stop server
```

### State

Two variables, no persistence needed:

- `claude-graph--process` — the trellis server process (or nil)
- `claude-graph--port` — fixed at 19847 (customizable via defcustom)

### Buffer

- Name: `*claude:graph*`
- Mode: `xwidget-webkit-mode` (built-in, no custom mode needed)
- Kill hook: sends SIGTERM to `claude-graph--process`, sets it to nil

### Keybinding

Bind `g` in `claude-dashboard-mode-map` to `claude-graph-open`. This keeps the graph accessible from mission control without polluting the global leader map.

### Module Architecture

```
claude-graph.el      # No deps on Doom — just xwidget-webkit + process management
    ↑
claude-dashboard.el  # Adds `g` keybinding to dashboard mode map
    ↑
claude.el            # Requires claude-graph
```

`claude-graph.el` has no dependency on other claude modules. It can run standalone if needed.

## Files Changed

| File | Change |
|------|--------|
| `claude-graph.el` (new) | ~50 lines: process management, xwidget buffer, kill hook |
| `claude-dashboard.el` | Add `g` keybinding in mode map |
| `claude.el` | Add `(require 'claude-graph)` |

## Error Handling

- **No xwidgets**: `claude-graph-open` checks `(featurep 'xwidget-internal)`, messages user if missing
- **Port in use**: Parse trellis stderr, message user to check for stale process
- **Process dies**: Sentinel cleans up `claude-graph--process`, next `g` press starts fresh
