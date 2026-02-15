# Inputs

## From plans

### From: active/graph-headless
- `--no-open` flag on `trellis graph` command
- Stdout contract: `Serving DAG viewer at http://localhost:<port>`

## From existing code

### emacs/doom/modules/claude/claude.el
- Module entry point — will add `(require 'claude-graph)`

### emacs/doom/modules/claude/claude-dashboard.el
- Dashboard mode map — will bind `g` to `claude-graph-open`

### xwidget-webkit (Emacs built-in)
- `xwidget-webkit-browse-url` — opens URL in embedded WebKit buffer
- `(featurep 'xwidget-internal)` — runtime check for xwidgets support
