# Outputs

## New file: claude-graph.el
- `claude-graph-open` interactive command — starts server if needed, opens xwidget buffer
- `claude-graph-close` interactive command — kills buffer and server
- `claude-graph-port` defcustom (default 19847)
- Server process lifecycle management (start, sentinel, cleanup)
- xwidgets feature check with user-facing error message

## Updated: claude-dashboard.el
- `g` keybinding in dashboard mode map bound to `claude-graph-open`

## Updated: claude.el
- `(require 'claude-graph)` added to module requires
