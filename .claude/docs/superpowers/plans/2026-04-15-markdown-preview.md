# Markdown 预览（pandoc + eww）实现计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 修复 `C-c C-c p` / `C-c C-c l` 报错，用 pandoc 作为转换后端、eww 作为预览窗口实现 Emacs 内 Markdown live 预览。

**Architecture:** 安装系统依赖 pandoc，然后扩展 `modules/init-lsp.el` 中已有的 `use-package markdown-mode` 块，设置 `markdown-command` 和 `markdown-live-preview-window-function` 两个变量。

**Tech Stack:** Emacs Lisp, markdown-mode (MELPA), pandoc (Homebrew), eww (Emacs 内置)

---

### Task 1: 安装系统依赖 pandoc

**Files:**
- 无文件变更（系统命令）

- [ ] **Step 1: 安装 pandoc**

在终端执行（用户自行运行，不在 Emacs 内执行）：

```bash
brew install pandoc
```

- [ ] **Step 2: 验证安装成功**

```bash
pandoc --version
```

预期输出首行类似：
```
pandoc 3.x.x
```

- [ ] **Step 3: 验证转换功能正常**

```bash
echo "# Hello\n\nThis is **markdown**." | pandoc -f markdown -t html5 --standalone
```

预期：输出完整 HTML 文档，包含 `<h1>Hello</h1>` 和 `<strong>markdown</strong>`。

---

### Task 2: 扩展 markdown-mode 配置

**Files:**
- Modify: `modules/init-lsp.el:70-72`

- [ ] **Step 1: 将 `use-package markdown-mode` 替换为完整配置**

将 `modules/init-lsp.el` 第 70-72 行：

```elisp
(use-package markdown-mode
  :ensure t
  :defer t)
```

替换为：

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

- [ ] **Step 2: 在 Emacs 中重新加载模块**

在 Emacs 执行：

```
M-x load-file RET ~/.emacs.d/modules/init-lsp.el RET
```

无报错即为成功。

- [ ] **Step 3: 打开一个 .md 文件验证 live preview**

1. 打开任意 `.md` 文件
2. 按 `C-c C-c l` 开启 `markdown-live-preview-mode`
3. 预期：右侧自动分屏，打开 eww buffer 并渲染 HTML
4. 编辑文档内容并保存（`C-x C-s`）
5. 预期：eww buffer 自动刷新显示最新内容

- [ ] **Step 4: 验证 `C-c C-c p` 也正常工作**

1. 按 `C-c C-c l` 关闭 live preview
2. 按 `C-c C-c p` 触发一次性预览
3. 预期：eww buffer 中展示渲染结果，不再报 `markdown is not found` 错误

- [ ] **Step 5: Commit**

```bash
git add modules/init-lsp.el
git commit -m "feat: 配置 markdown-mode 使用 pandoc + eww 实现 live preview"
```
