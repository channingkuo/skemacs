<!-- de861a33-be51-488c-9787-37845b729c37 -->
---
todos:
  - id: "create-modeline"
    content: "新建 modules/init-modeline.el，配置 mood-line 包"
    status: pending
isProject: false
---
# Add mood-line Module

## 新建文件

`modules/init-modeline.el`

## 内容结构

- `use-package mood-line` with `:ensure t`
- 在 `:config` 中启用 `(mood-line-mode)`
- 配置 `mood-line-format` 使用内置 segment 组合：
  - 左侧：modal 状态、buffer 名称、修改标记、只读标记、项目/文件路径
  - 右侧：major mode、行列号、VC branch、编码
- 使用 `mood-line-glyphs-unicode` 作为字形集（无需 nerd-icons，终端/GUI 均兼容）

## 关键代码参考

`mood-line` v2 的格式配置方式：

```elisp
(setq mood-line-format
      (mood-line-defformat
       :left
       (((mood-line-segment-buffer-status) . " ")
        ((mood-line-segment-buffer-name)   . "  ")
        ((mood-line-segment-anzu)          . "  ")
        ((mood-line-segment-multiple-cursors) . "  ")
        ((mood-line-segment-cursor-position) . " ")
        ((mood-line-segment-scroll)        . " "))
       :right
       (((mood-line-segment-vc)            . "  ")
        ((mood-line-segment-major-mode)    . "  ")
        ((mood-line-segment-misc-info)     . "  ")
        ((mood-line-segment-checker)       . "  ")
        ((mood-line-segment-process)       . "  "))))
```

- 字形集设置：`(setq mood-line-glyph-alist mood-line-glyphs-unicode)`

## 注意事项

- mood-line 是纯 UI 模块，无需快捷键
- 需在 `:config` 调用 `(mood-line-mode)` 全局启用（不用 `:hook`）
- 与 `core-ui.el` 中已有的 `(column-number-mode t)` 共存，无冲突
