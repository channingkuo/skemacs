# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Skemacs is a modular, high-performance Emacs configuration framework with:
- **Independent modules**: Each module is fully self-contained (package declaration + config + keybindings)
- **Startup timing**: Every module's load time is recorded; view with `M-x skemacs/show-load-times`
- **Error isolation**: A single module failure won't break others (`condition-case` wrapped)
- **Dynamic splash screen**: Real-time loading progress displayed at startup
- **Emacs 29.1+ required**: Leverages built-in use-package and native compilation

## Architecture

### Startup Flow

```
early-init.el → init.el → custom.el → splash screen → core/*.el → modules/*.el → local/*.el → Press Enter → dired
```

1. **early-init.el** — GC optimization (800MB threshold), disable package autoload, native compilation settings
2. **init.el** — Bootstrap core/ into load-path, require core-load-paths and core-module (without timing)
3. **custom.el** — Loaded early so `skemacs-disabled-modules` and other Custom settings take effect before module loading
4. **Splash screen** — Display banner and real-time module loading progress
5. **emacs-startup-hook** — Restore GC to 16MB, show summary, wait for Enter (or skip wait if started with `emacs file.txt`)

### Core System Architecture

The configuration is divided into two main parts:

**core/** — Core infrastructure (no third-party dependencies):
- `core-load-paths.el` — Path constants and load-path setup
- `core-module.el` — **Module loading system** with timing, error isolation, and splash screen management
- `core-packages.el` — use-package + package.el configuration (direct initialization)
- `core-ui.el` — Basic UI configuration
- `core-editor.el` — Editing behavior (electric-pair, show-paren, delete-selection, etc.)
- `core-funcs.el` — Utility functions in `skemacs/` namespace
- `core-keybindings.el` — Prefix key definitions and global keybindings

**modules/** — Feature modules (auto-scanned, each file is self-contained):
- Each module follows a standard template with `use-package`
- Modules are loaded in alphabetical order by default
- Can be controlled via `skemacs-module-list` or `skemacs-disabled-modules`

### Module Loading System

The module system (defined in `core/core-module.el`) provides:

- **Auto-scanning**: Automatically loads all `.el` files in `modules/` directory
- **Timing collection**: Records load time for each module in `skemacs-load-times`
- **Error isolation**: Each module wrapped in `condition-case` to prevent cascading failures
- **Splash integration**: Real-time display of module loading status during startup
- **Selective loading**: Configure via:
  - `skemacs-module-list` — Specify modules to load (nil = auto-scan)
  - `skemacs-disabled-modules` — Exclude specific modules (highest priority)

### Key Variables and Functions

| Variable/Function | Purpose |
|------------------|---------|
| `skemacs-module-list` | Module list to load; nil = auto-scan modules/ |
| `skemacs-disabled-modules` | Disabled module list (highest priority) |
| `skemacs-load-times` | Timing data `(("name" . seconds) ...)` |
| `skemacs-module-errors` | Error records `(("name" . "message") ...)` |
| `skemacs-load-module` | Load a single module with timing and error isolation |
| `skemacs-load-core-module` | Load a core module from `core/` directory |
| `skemacs-load-all-modules` | Load all enabled modules (including `local/` directory) |
| `skemacs/show-load-times` | Interactive command to show formatted timing report |

## Development Workflow

### Testing Configuration Changes

Start Emacs with debug mode to see full error backtraces:
```bash
emacs --debug-init
```

Test with vanilla Emacs (no configuration):
```bash
emacs -Q
```

### Module Development

When adding a new module to `modules/`:

1. **Create module file** following the template:
```elisp
;;; init-xxx.el --- Short description -*- lexical-binding: t -*-
;;; Commentary:
;; Detailed description of module purpose and included packages.

;;; Code:

(use-package some-package
  :ensure t
  :defer t                              ; Lazy load when possible
  :hook (some-mode . some-package-mode)
  :bind
  (("C-j x" . some-command)
   :map some-mode-map
   ("C-c x" . another-command))
  :init
  ;; which-key descriptions
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-j x" "description"))
  :config
  (setq some-option t))

(provide 'init-xxx)
;;; init-xxx.el ends here
```

2. **Module naming**: Use `init-<feature>.el` format
3. **Always provide**: End with `(provide 'init-xxx)` matching filename
4. **Keybindings**: Use `:bind` in use-package for automatic tracking via `describe-personal-keybindings`
5. **Defer loading**: Add `:defer t` and use `:hook` or `:commands` for lazy loading
6. **which-key integration**: Add descriptions for new keybindings

### Prefix Keys

Prefix keys are defined in `core-keybindings.el`. Modules should bind under these prefixes:

| Prefix | Purpose | Example Usage |
|--------|---------|---------------|
| `C-j` | Main Skemacs prefix | `C-j t` → show load times |
| `C-j o` | Org-related commands | `C-j o a` → org-agenda |
| `C-j p` | Project commands | `C-j p f` → find file in project |
| `C-j s` | Search (Consult) | `C-j s l` → consult-line |
| `C-j f` | File tree (Treemacs) | `C-j f f` → treemacs |
| `C-j g` | Git (Magit) | `C-j g g` → magit-status |
| `C-j j` | Jump (Avy) | `C-j j j` → avy-goto-char |
| `C-j v` | Terminal (Vterm) | `C-j v v` → vterm |
| `C-j a` | AI Agent (agent-shell) | `C-j a a` → agent-shell |
| `C-j c` | Claude Code IDE | `C-j c c` → claude-code |
| `C-j m` | Multiple Cursors | `C-j m n` → mark next |
| `C-j n` | Toggle line numbers | `C-j n` → display-line-numbers-mode |
| `C-j r` | Toggle line number type | `C-j r` → relative/absolute |
| `C-x w` | Window operations | `C-x w s` → split window |
| `C-x b` | Buffer operations | `C-x b b` → switch buffer |

### Viewing Module Performance

After startup or at runtime:
```elisp
M-x skemacs/show-load-times
```
Or use shortcut: `C-j t`

Modules marked `SLOW` (>0.5s) should be optimized with `:defer t` for lazy loading.

### Disabling Modules

To temporarily disable a module without deleting the file, set in `custom.el` (or via `M-x customize-variable`):

```elisp
(setq skemacs-disabled-modules
      '("init-vterm"
        "init-treemacs"))
```

### Package Management

Package sources are configured in `core-packages.el`:
- GNU ELPA (elpa.gnu.org)
- NonGNU ELPA (elpa.nongnu.org)
- MELPA (melpa.org)

Refresh package list:
```elisp
M-x package-refresh-contents
```

### GC Strategy

| Phase | gc-cons-threshold | Purpose |
|-------|------------------|---------|
| During startup | 800MB | Reduce GC pauses for fast startup |
| After startup | 16MB | Normal operation, free memory |

## Important Implementation Details

### Package Initialization

`core-packages.el` calls `(package-initialize)` directly at load time to scan `elpa/` and activate installed packages. `use-package` is built-in (Emacs 29+) with `use-package-always-ensure t`.

### Splash Screen System

The splash screen (`core-module.el`) provides real-time feedback:
- Displays banner from `banners/` directory (controlled by `skemacs-banner-file`)
- Stats summary (Total/loaded/disabled/errors) displayed above the table (placeholder "Loading modules..." during loading, replaced with actual stats after completion)
- Shows table with module name, load time, and status (OK/SLOW/ERROR/MISS/SKIP)
- Error details displayed below the table; shows "no error" if all modules loaded successfully
- Enters `recursive-edit` allowing full navigation before pressing Enter
- Automatically opens dired after dismissing splash (or switches to file buffer in file-mode)

### Local Configuration Loading

After loading all feature modules, `skemacs-load-all-modules` also auto-scans and loads `.el` files from `local/` directory. This allows per-machine customizations without modifying tracked files.

### Error Handling

All modules loaded via `skemacs-load-module` are wrapped in `condition-case`:
- Errors are recorded in `skemacs-module-errors`
- Failed modules don't prevent subsequent modules from loading
- Error details are shown in splash screen and load report

## File Locations

- **Core modules**: `~/.emacs.d/core/`
- **Feature modules**: `~/.emacs.d/modules/`
- **Local configs**: `~/.emacs.d/local/` (not tracked by git)
- **Custom file**: `~/.emacs.d/custom.el` (auto-generated, not tracked)
- **Banners**: `~/.emacs.d/banners/`
- **Package cache**: `~/.emacs.d/elpa/`

## Common Issues

### Startup Errors
1. Check `*Messages*` and `*Warnings*` buffers
2. Module errors are recorded but don't break startup
3. Use `emacs --debug-init` for full backtrace

### Slow Startup
1. Run `M-x skemacs/show-load-times` to identify slow modules
2. Add `:defer t` to slow modules for lazy loading
3. Use `:hook` or `:commands` instead of immediate loading

### Package Install Failures
1. Check network connection
2. Uncomment proxy settings in `core-packages.el` if needed
3. Run `M-x package-refresh-contents`
4. Delete `~/.emacs.d/elpa/` and restart if corrupted
