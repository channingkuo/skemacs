---
name: Vue3 LSP Configuration
overview: Configure lsp-bridge with Volar + vtsls multiserver for Vue 3 / TypeScript development. Node.js and Python both embedded in .emacs.d for full self-containment.
todos:
  - id: setup-nodejs
    content: 下载 Node.js v22 LTS (darwin-arm64) 到 ~/.emacs.d/nodejs/，然后安装 LSP 全局包
    status: pending
  - id: setup-python-venv
    content: 用 uv 创建 ~/.emacs.d/.venv 虚拟环境并安装 lsp-bridge Python 依赖
    status: pending
  - id: clone-lsp-bridge
    content: Git clone lsp-bridge 到 ~/.emacs.d/lsp-bridge/ 目录
    status: pending
  - id: create-init-web
    content: 创建 modules/init-web.el — web-mode (.vue/.html) + typescript-mode (.ts/.tsx)
    status: completed
  - id: create-init-lsp
    content: 创建 modules/init-lsp.el — lsp-bridge 配置（内置 Node/Python 路径、Volar+vtsls、快捷键）
    status: completed
  - id: update-gitignore
    content: 更新 .gitignore 添加 nodejs/ 和 .venv/
    status: completed
  - id: create-vtsls-override
    content: 创建 lsp-bridge-langserver/vtsls.json 指向内置 nodejs 中的 @vue/typescript-plugin
    status: pending
isProject: false
---

# Vue 3 + TypeScript LSP 开发环境配置方案

## 设计思路

将 Node.js 和 Python 都内置到 `~/.emacs.d/` 中，使 Emacs 配置完全自包含：

```
~/.emacs.d/
  nodejs/          # 内置 Node.js v22 LTS + LSP 服务器
  .venv/           # 内置 Python venv (lsp-bridge 依赖)
  lsp-bridge/      # lsp-bridge 源码 (git clone)
  lsp-bridge-langserver/  # 用户自定义 langserver 配置
  modules/
    init-web.el    # web-mode + typescript-mode
    init-lsp.el    # lsp-bridge 完整配置
```

好处：不依赖 fnm PATH、不需要 exec-path-from-shell、GUI/终端启动都正常、迁移方便。

---

## 第一步：安装外部依赖（Shell 命令）

### 1.1 内置 Node.js

```bash
mkdir -p ~/.emacs.d/nodejs
curl -L https://nodejs.org/dist/v22.14.0/node-v22.14.0-darwin-arm64.tar.xz \
  | tar xJ -C ~/.emacs.d/nodejs --strip-components=1
```

安装 LSP 服务器到内置 Node.js：

```bash
~/.emacs.d/nodejs/bin/npm install -g @vue/language-server typescript @vtsls/language-server
```

安装后，二进制文件位于 `~/.emacs.d/nodejs/bin/`，npm 包位于 `~/.emacs.d/nodejs/lib/node_modules/`。

### 1.2 Python 虚拟环境

```bash
cd ~/.emacs.d
uv venv .venv
uv pip install --python .venv/bin/python epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
```

### 1.3 Clone lsp-bridge

[.gitignore](.gitignore) 第 43 行已有 `lsp-bridge/` 条目：

```bash
cd ~/.emacs.d
git clone --depth 1 https://github.com/manateelazycat/lsp-bridge.git
```

---

## 第二步：创建 vtsls.json 覆盖配置

由于内置 Node.js 的路径与 lsp-bridge 默认的 `/opt/homebrew/...` 不同，需要创建覆盖配置。

**新建** `~/.emacs.d/lsp-bridge-langserver/vtsls.json`：

```json
{
  "name": "vtsls",
  "command": ["vtsls", "--stdio"],
  "languageId": "typescript",
  "fileExtensions": ["ts", "tsx"],
  "rootPatterns": ["tsconfig.json", "package.json"],
  "initializationOptions": {
    "typescript": {
      "tsdk": ""
    }
  },
  "settings": {
    "vtsls": {
      "tsserver": {
        "globalPlugins": [
          {
            "name": "@vue/typescript-plugin",
            "location": "~/.emacs.d/nodejs/lib/node_modules/@vue/language-server",
            "languages": ["vue"],
            "configNamespace": "typescript"
          }
        ]
      }
    }
  }
}
```

`location` 指向内置 Node.js 中 `@vue/language-server` 的实际安装位置。实际写入时会用 `expand-file-name` 展开 `~` 为绝对路径。

---

## 第三步：创建 Emacs 模块

### 模块 1: [modules/init-web.el](modules/init-web.el)

```elisp
;;; init-web.el --- Web development modes -*- lexical-binding: t -*-
;;; Commentary:
;; Web-mode for .vue/.html files, TypeScript mode for .ts/.tsx files.
;; Indentation set to 2 spaces (frontend standard).

;;; Code:

(use-package web-mode
  :ensure t
  :mode
  (("\\.vue\\'" . web-mode)
   ("\\.html\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2))

(provide 'init-web)
;;; init-web.el ends here
```

### 模块 2: [modules/init-lsp.el](modules/init-lsp.el)

```elisp
;;; init-lsp.el --- LSP configuration with lsp-bridge -*- lexical-binding: t -*-
;;; Commentary:
;; lsp-bridge for LSP support with embedded Node.js and Python.
;; Node.js v22 LTS at ~/.emacs.d/nodejs/ (LSP servers installed there).
;; Python venv at ~/.emacs.d/.venv/ (lsp-bridge async backend).
;; Uses Volar + vtsls multiserver for Vue 3 + TypeScript.

;;; Code:

;; ---------------------------------------------------------------------------
;; Embedded Node.js: prepend to exec-path and PATH
;; ---------------------------------------------------------------------------
(let ((node-bin (expand-file-name "nodejs/bin" user-emacs-directory)))
  (when (file-directory-p node-bin)
    (add-to-list 'exec-path node-bin)
    (setenv "PATH" (concat node-bin ":" (getenv "PATH")))))

;; ---------------------------------------------------------------------------
;; lsp-bridge dependencies
;; ---------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package markdown-mode
  :ensure t
  :defer t)

;; ---------------------------------------------------------------------------
;; lsp-bridge
;; ---------------------------------------------------------------------------
(use-package lsp-bridge
  :load-path "lsp-bridge"
  :hook
  ((web-mode . lsp-bridge-mode)
   (typescript-mode . lsp-bridge-mode)
   (js-mode . lsp-bridge-mode))
  :bind
  (("M-." . lsp-bridge-find-def)
   ("M-," . lsp-bridge-find-def-return)
   ("M-?" . lsp-bridge-find-references)
   :map lsp-bridge-mode-map
   ("C-j l r" . lsp-bridge-rename)
   ("C-j l a" . lsp-bridge-code-action)
   ("C-j l d" . lsp-bridge-show-documentation)
   ("C-j l f" . lsp-bridge-find-def)
   ("C-j l i" . lsp-bridge-find-impl)
   ("C-j l e" . lsp-bridge-diagnostic-list))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-j l" "lsp"))
  :config
  ;; Python: use embedded venv
  (setq lsp-bridge-python-command
        (expand-file-name ".venv/bin/python" user-emacs-directory))
  ;; User langserver dir (vtsls.json override for embedded Node.js path)
  (setq lsp-bridge-user-langserver-dir
        (expand-file-name "lsp-bridge-langserver" user-emacs-directory))
  ;; Vue: Volar + vtsls multiserver
  (setq lsp-bridge-multi-lang-server-extension-list
        '((("vue") . "volar_vtsls")))
  ;; Diagnostics
  (setq lsp-bridge-enable-diagnostics t))

(provide 'init-lsp)
;;; init-lsp.el ends here
```

核心设计：

- 文件开头将 `nodejs/bin` 加入 `exec-path` 和 `PATH`，确保 lsp-bridge 能找到所有 LSP 命令
- `lsp-bridge-python-command` 指向 `.venv/bin/python`
- `lsp-bridge-user-langserver-dir` 指向自定义目录（存放 vtsls.json 覆盖）
- Vue 文件使用 `volar_vtsls` 多服务器融合

---

## 第四步：更新 .gitignore

在 [.gitignore](.gitignore) 中添加（`lsp-bridge/` 已存在于第 43 行）：

```gitignore
nodejs/
.venv/
lsp-bridge-langserver/
```

---

## 快捷键汇总

- **M-.** — 跳转到定义（.vue / .ts / .tsx）
- **M-,** — 跳转返回
- **M-?** — 查找引用
- **C-j l d** — 显示文档
- **C-j l r** — 重命名符号
- **C-j l a** — 代码操作
- **C-j l i** — 查找实现
- **C-j l e** — 诊断列表
- 智能补全 — 自动弹出（lsp-bridge ACM 前端）

---

## 文件变更清单

- **新建** `modules/init-web.el` — web-mode + typescript-mode
- **新建** `modules/init-lsp.el` — lsp-bridge 配置（含内置 Node/Python 路径设置）
- **新建** `lsp-bridge-langserver/vtsls.json` — @vue/typescript-plugin 路径覆盖
- **修改** [.gitignore](.gitignore) — 添加 `nodejs/`、`.venv/`、`lsp-bridge-langserver/`

