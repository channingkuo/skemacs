# Markdown 预览功能设计文档

**日期**：2026-04-15
**状态**：已审阅

---

## 背景与问题

`markdown-mode` 的 `C-c C-c p`（`markdown-preview`）和 `C-c C-c l`（`markdown-live-preview-mode`）依赖变量 `markdown-command`，其默认值为字符串 `"markdown"`（系统可执行文件）。系统未安装该工具，导致报错：

```
user-error: Markdown command markdown is not found
```

## 目标

在 Emacs 内以 live 模式预览 Markdown，保存时自动刷新，无需切换到外部浏览器。

## 方案

使用 **pandoc** 作为 Markdown 转换后端，配合 `markdown-live-preview-mode` 在 eww buffer 中渲染。

### 系统依赖

```bash
brew install pandoc
```

### 配置变更

**文件**：`modules/init-lsp.el`，扩展现有 `use-package markdown-mode` 块（当前 line 70-72）：

```elisp
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc -f markdown -t html5 --standalone"
        markdown-live-preview-window-function
          #'markdown-live-preview-window-eww))
```

### 关键变量说明

| 变量 | 值 | 作用 |
|------|----|------|
| `markdown-command` | `"pandoc -f markdown -t html5 --standalone"` | 将 .md 转为独立 HTML |
| `markdown-live-preview-window-function` | `markdown-live-preview-window-eww` | live preview 在 eww buffer 中展示 |

### 使用方式

- `C-c C-c l` — 开启/关闭 live preview（eww 实时渲染，保存自动刷新）
- `C-c C-c p` — 一次性预览（同样通过 pandoc 转换）

## 不变内容

- 快捷键注释（line 45-68）保持不变
- 不新建模块文件，改动在现有 `init-lsp.el` 内
- 其他 LSP / yasnippet 配置不受影响
