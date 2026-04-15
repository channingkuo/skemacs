---
name: Add TailwindCSS to Vue
overview: Add TailwindCSS language server support to the lsp-bridge Vue multiserver configuration, enabling class name completion and hover documentation in Vue template sections.
todos:
  - id: setup-lsp-npm
    content: Add @tailwindcss/language-server to npm install in setup-lsp.sh
    status: completed
  - id: setup-lsp-multiserver
    content: Add setup_multiserver_overrides() function in setup-lsp.sh to generate lsp-bridge-multiserver/volar_vtsls_tailwindcss.json
    status: completed
  - id: setup-lsp-summary
    content: Update setup-lsp.sh main flow and summary output
    status: completed
  - id: init-lsp-config
    content: "Update init-lsp.el: add user-multiserver-dir, change Vue multiserver to volar_vtsls_tailwindcss"
    status: completed
  - id: gitignore
    content: Add lsp-bridge-multiserver/ to .gitignore
    status: completed
isProject: false
---

# Add TailwindCSS Support for Vue Files in lsp-bridge

## Background

Currently Vue files use the `volar_vtsls` multiserver (Volar + vtsls). lsp-bridge already ships with:

- `langserver/tailwindcss.json` -- TailwindCSS language server config (command: `tailwindcss-language-server --stdio`)
- `multiserver/css_tailwindcss.json` / `multiserver/html_tailwindcss.json` -- examples of combining TailwindCSS with other servers

We need to add TailwindCSS as a third server alongside Volar and vtsls for Vue files.

## Changes

### 1. [setup-lsp.sh](setup-lsp.sh) -- Install TailwindCSS language server + generate multiserver config

**a) Add `@tailwindcss/language-server` to npm install** (line 91-94):

```bash
"${NODE_DIR}/bin/node ${NODE_DIR}/bin/npm" install -g \
    @vue/language-server \
    typescript \
    @vtsls/language-server \
    @tailwindcss/language-server
```

**b) Add a new function `setup_multiserver_overrides()` to generate the Vue multiserver config** that combines volar + vtsls + tailwindcss:

- Create directory `lsp-bridge-multiserver/`
- Generate `volar_vtsls_tailwindcss.json`:

```json
{
  "default": "vtsls",
  "servers": ["volar", "vtsls", "tailwindcss"],
  "completion": ["volar", "vtsls", "tailwindcss"],
  "completion_item_resolve": ["volar", "vtsls", "tailwindcss"],
  "diagnostics": ["volar", "vtsls", "tailwindcss"],
  "code_action": ["volar", "vtsls", "tailwindcss"],
  "hover": ["volar", "tailwindcss"]
}
```

This ensures:

- Completion includes TailwindCSS class names alongside Volar/vtsls completions
- Hover shows TailwindCSS class meaning (from `tailwindcss`) plus Vue component docs (from `volar`)
- Diagnostics from all three servers

**c) Update the main flow and summary output** to include the new step and TailwindCSS info.

### 2. [modules/init-lsp.el](modules/init-lsp.el) -- Configure lsp-bridge to use new multiserver

Three changes in the `:config` section:

**a) Set `lsp-bridge-user-multiserver-dir`** (new, alongside existing `lsp-bridge-user-langserver-dir`):

```elisp
(setq lsp-bridge-user-multiserver-dir
      (expand-file-name "lsp-bridge-multiserver" user-emacs-directory))
```

**b) Update Vue multiserver name** from `"volar_vtsls"` to `"volar_vtsls_tailwindcss"`:

```elisp
(setq lsp-bridge-multi-lang-server-extension-list
      '((("vue") . "volar_vtsls_tailwindcss")))
```

**c) Set `lsp-bridge-get-language-id`** to return correct languageId for TailwindCSS in Vue files. The built-in default handler already returns `extension-name` for tailwindcss (which would be `"vue"`), but Vue's TailwindCSS needs languageId `"html"` to trigger proper completions in `<template>`:

```elisp
(setq lsp-bridge-get-language-id
      (lambda (_project-path _file-path server-name extension-name)
        (when (string-equal server-name "tailwindcss")
          (if (string-equal extension-name "jsx")
              "javascriptreact"
            extension-name))))
```

Note: The built-in fallback already does this, so we only need to add this if we want to customize (e.g., map `"vue"` to `"html"` for tailwindcss). The TailwindCSS language server accepts `"vue"` as a valid languageId, so the default should work. We will keep the default behavior.

### 3. [.gitignore](.gitignore) -- Add new directory

Add `lsp-bridge-multiserver/` to `.gitignore` (matching existing `lsp-bridge-langserver/`).

## Notes

- TailwindCSS completion only works in projects with a `tailwind.config.js` / `tailwind.config.ts` (the langserver has `"support-single-file": false`)
- After making these changes, run `bash setup-lsp.sh` to install the TailwindCSS language server and generate configs
- Restart Emacs (or `M-x lsp-bridge-restart-process`) to pick up the new configuration

