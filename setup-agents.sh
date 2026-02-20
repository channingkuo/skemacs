#!/usr/bin/env bash
# ============================================================================
# setup-agents.sh — 一键安装 ACP 代理适配器
# ============================================================================
#
# 功能：
#   使用项目内置的 Node.js (nodejs/) 安装 ACP (Agent Client Protocol) 适配器，
#   供 Emacs agent-shell 模块调用，支持以下 AI 编码代理：
#     - Claude Code   (Anthropic)  → claude-agent-acp
#     - Cursor Agent               → cursor-agent-acp
#     - Gemini CLI    (Google)     → gemini
#
# 前置条件：
#   需要先运行 setup-lsp.sh 安装内置 Node.js
#
# 用法：
#   cd ~/.emacs.d && bash setup-agents.sh
#
# ============================================================================

set -euo pipefail

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Emacs 配置根目录（脚本所在目录）
EMACS_DIR="$(cd "$(dirname "$0")" && pwd)"

# 配置项
NODE_DIR="${EMACS_DIR}/nodejs"
NPM="${NODE_DIR}/bin/npm"

# ============================================================================
# 工具函数
# ============================================================================

info()  { echo -e "${BLUE}[INFO]${NC} $*"; }
ok()    { echo -e "${GREEN}[OK]${NC} $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*"; exit 1; }

# ============================================================================
# Step 1: 检查内置 Node.js
# ============================================================================

check_nodejs() {
    info "=== Step 1: 检查内置 Node.js ==="

    if [[ ! -x "${NODE_DIR}/bin/node" ]]; then
        error "未找到内置 Node.js，请先运行: bash setup-lsp.sh"
    fi

    local node_version
    node_version=$("${NODE_DIR}/bin/node" --version 2>/dev/null || echo "unknown")
    ok "Node.js 已就绪 (${node_version})"
    info "  - node: ${NODE_DIR}/bin/node"
    info "  - npm:  ${NPM}"
}

# ============================================================================
# Step 2: 安装 ACP 代理适配器
# ============================================================================

setup_agents() {
    info "=== Step 2: 安装 ACP 代理适配器 ==="

    info "安装 @zed-industries/claude-agent-acp ..."
    "${NODE_DIR}/bin/node" "${NPM}" install -g @zed-industries/claude-agent-acp

    info "安装 @blowmage/cursor-agent-acp ..."
    "${NODE_DIR}/bin/node" "${NPM}" install -g @blowmage/cursor-agent-acp

    info "安装 @google/gemini-cli ..."
    "${NODE_DIR}/bin/node" "${NPM}" install -g @google/gemini-cli

    ok "ACP 代理适配器安装完成"
}

# ============================================================================
# Step 3: 验证安装
# ============================================================================

verify_agents() {
    info "=== Step 3: 验证安装 ==="

    local all_ok=true

    for cmd in claude-agent-acp cursor-agent-acp gemini; do
        if [[ -x "${NODE_DIR}/bin/${cmd}" ]] || [[ -L "${NODE_DIR}/bin/${cmd}" ]]; then
            ok "${cmd} 已安装 → ${NODE_DIR}/bin/${cmd}"
        else
            warn "${cmd} 未找到于 ${NODE_DIR}/bin/"
            all_ok=false
        fi
    done

    if [[ "${all_ok}" == true ]]; then
        ok "所有 ACP 代理适配器验证通过"
    else
        warn "部分适配器安装可能不完整，请检查上方输出"
    fi
}

# ============================================================================
# 主流程
# ============================================================================

main() {
    echo ""
    echo -e "${BLUE}╔══════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║     Skemacs ACP 代理适配器一键安装脚本          ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════╝${NC}"
    echo ""
    info "Emacs 目录: ${EMACS_DIR}"
    echo ""

    check_nodejs
    echo ""
    setup_agents
    echo ""
    verify_agents

    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║     安装完成！                                  ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════╝${NC}"
    echo ""
    info "已安装的 ACP 适配器:"
    info "  - claude-agent-acp:  ${NODE_DIR}/bin/claude-agent-acp"
    info "  - cursor-agent-acp: ${NODE_DIR}/bin/cursor-agent-acp"
    info "  - gemini:           ${NODE_DIR}/bin/gemini"
    echo ""
    info "认证说明（首次使用前需完成）:"
    info "  - Claude Code:  启动 agent-shell 时自动打开浏览器登录"
    info "  - Cursor Agent: 先在终端执行 cursor-agent login"
    info "  - Gemini CLI:   启动 agent-shell 时自动打开浏览器登录"
    echo ""
    info "下一步：启动 Emacs，按 C-j a 使用 AI Agent"
    echo ""
}

main "$@"
