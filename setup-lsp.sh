#!/usr/bin/env bash
# ============================================================================
# setup-lsp.sh — 一键配置 lsp-bridge 开发环境
# ============================================================================
#
# 功能：
#   1. 下载内置 Node.js v22 LTS (darwin-arm64) 到 nodejs/
#   2. 安装 LSP 服务器（Volar + vtsls + TypeScript）
#   3. 创建 Python 虚拟环境并安装 lsp-bridge 依赖
#   4. Clone lsp-bridge 源码
#   5. 生成 vtsls.json 覆盖配置（修正 @vue/typescript-plugin 路径）
#
# 用法：
#   cd ~/.emacs.d && bash setup-lsp.sh
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
NODE_VERSION="22.14.0"
NODE_ARCH="darwin-arm64"
PYTHON_VERSION="3.13"
NODE_DIR="${EMACS_DIR}/nodejs"
VENV_DIR="${EMACS_DIR}/.venv"
LSP_BRIDGE_DIR="${EMACS_DIR}/lsp-bridge"
LANGSERVER_DIR="${EMACS_DIR}/lsp-bridge-langserver"

# ============================================================================
# 工具函数
# ============================================================================

info()  { echo -e "${BLUE}[INFO]${NC} $*"; }
ok()    { echo -e "${GREEN}[OK]${NC} $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*"; exit 1; }

check_command() {
    if ! command -v "$1" &>/dev/null; then
        error "$1 未安装，请先安装后重试"
    fi
}

# ============================================================================
# Step 1: 下载并安装内置 Node.js
# ============================================================================

setup_nodejs() {
    info "=== Step 1: 安装内置 Node.js v${NODE_VERSION} ==="

    if [[ -x "${NODE_DIR}/bin/node" ]]; then
        local current_version
        current_version=$("${NODE_DIR}/bin/node" --version 2>/dev/null || echo "unknown")
        warn "Node.js 已存在 (${current_version})，跳过下载"
        warn "如需重新安装，请先删除 ${NODE_DIR}/ 目录"
    else
        check_command curl

        local node_tarball="node-v${NODE_VERSION}-${NODE_ARCH}.tar.xz"
        local node_url="https://nodejs.org/dist/v${NODE_VERSION}/${node_tarball}"
        info "node_url: ${node_url}"

        info "下载 Node.js v${NODE_VERSION} (${NODE_ARCH})..."
        mkdir -p "${NODE_DIR}"
        curl -L "${node_url}" | tar xJ -C "${NODE_DIR}" --strip-components=1

        if [[ -x "${NODE_DIR}/bin/node" ]]; then
            ok "Node.js 安装成功: $("${NODE_DIR}/bin/node" --version)"
        else
            error "Node.js 安装失败"
        fi
    fi

    # 安装 LSP 服务器
    info "安装 LSP 服务器..."
    "${NODE_DIR}/bin/node ${NODE_DIR}/bin/npm" install -g \
        @vue/language-server \
        typescript \
        @vtsls/language-server

    ok "LSP 服务器安装完成"
    info "  - vue-language-server: $("${NODE_DIR}/bin/vue-language-server" --version 2>/dev/null || echo 'installed')"
    info "  - vtsls: installed"
    info "  - typescript: $("${NODE_DIR}/bin/tsc" --version 2>/dev/null || echo 'installed')"
}

# ============================================================================
# Step 2: 创建 Python 虚拟环境
# ============================================================================

setup_python_venv() {
    info "=== Step 2: 创建 Python ${PYTHON_VERSION} 虚拟环境 ==="

    check_command uv

    # 检查已有 venv 的 Python 版本是否满足要求
    local need_recreate=false
    if [[ -x "${VENV_DIR}/bin/python" ]]; then
        local current_py_version
        current_py_version=$("${VENV_DIR}/bin/python" --version 2>&1 | grep -oE '[0-9]+\.[0-9]+')
        local required_minor=${PYTHON_VERSION#*.}
        local current_minor=${current_py_version#*.}
        local required_major=${PYTHON_VERSION%%.*}
        local current_major=${current_py_version%%.*}

        if [[ "${current_major}" -lt "${required_major}" ]] || \
           { [[ "${current_major}" -eq "${required_major}" ]] && [[ "${current_minor}" -lt "${required_minor}" ]]; }; then
            warn "当前 Python 版本 ${current_py_version} 低于要求的 ${PYTHON_VERSION}"
            warn "删除旧虚拟环境并重新创建..."
            rm -rf "${VENV_DIR}"
            need_recreate=true
        else
            info "Python 虚拟环境已存在 (${current_py_version})，更新依赖..."
        fi
    else
        need_recreate=true
    fi

    if [[ "${need_recreate}" == true ]] || [[ ! -x "${VENV_DIR}/bin/python" ]]; then
        info "创建虚拟环境 (Python ${PYTHON_VERSION})..."
        # uv 会自动下载指定版本的 Python（如果本地没有）
        uv venv --python "${PYTHON_VERSION}" "${VENV_DIR}"
    fi

    info "安装 lsp-bridge Python 依赖..."
    uv pip install --python "${VENV_DIR}/bin/python" \
        epc orjson sexpdata six setuptools paramiko \
        rapidfuzz watchdog packaging

    ok "Python 虚拟环境配置完成"
    info "  - Python: $("${VENV_DIR}/bin/python" --version)"
}

# ============================================================================
# Step 3: Clone lsp-bridge
# ============================================================================

setup_lsp_bridge() {
    info "=== Step 3: Clone lsp-bridge ==="

    check_command git

    if [[ -d "${LSP_BRIDGE_DIR}/.git" ]]; then
        warn "lsp-bridge 已存在，拉取最新代码..."
        cd "${LSP_BRIDGE_DIR}" && git pull --ff-only && cd "${EMACS_DIR}"
    else
        info "Cloning lsp-bridge..."
        git clone --depth 1 https://github.com/manateelazycat/lsp-bridge.git "${LSP_BRIDGE_DIR}"
    fi

    ok "lsp-bridge 就绪"
}

# ============================================================================
# Step 4: 生成 vtsls.json 覆盖配置
# ============================================================================

setup_vtsls_override() {
    info "=== Step 4: 生成 vtsls.json 覆盖配置 ==="

    local vue_plugin_location="${NODE_DIR}/lib/node_modules/@vue/language-server"

    if [[ ! -d "${vue_plugin_location}" ]]; then
        warn "@vue/language-server 未找到于 ${vue_plugin_location}"
        warn "vtsls.json 仍会生成，但 Vue 的 TypeScript 支持可能不工作"
    fi

    mkdir -p "${LANGSERVER_DIR}"

    cat > "${LANGSERVER_DIR}/vtsls.json" << VTSLS_EOF
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
            "location": "${vue_plugin_location}",
            "languages": ["vue"],
            "configNamespace": "typescript"
          }
        ]
      }
    }
  }
}
VTSLS_EOF

    ok "vtsls.json 已生成: ${LANGSERVER_DIR}/vtsls.json"
    info "  - @vue/typescript-plugin 路径: ${vue_plugin_location}"
}

# ============================================================================
# 主流程
# ============================================================================

main() {
    echo ""
    echo -e "${BLUE}╔══════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║     Skemacs LSP 开发环境一键配置脚本            ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════╝${NC}"
    echo ""
    info "Emacs 目录: ${EMACS_DIR}"
    echo ""

    setup_nodejs
    echo ""
    setup_python_venv
    echo ""
    setup_lsp_bridge
    echo ""
    setup_vtsls_override

    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║     配置完成！                                  ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════╝${NC}"
    echo ""
    info "已安装组件:"
    info "  Node.js:     ${NODE_DIR}/bin/node"
    info "  Python:      ${VENV_DIR}/bin/python"
    info "  lsp-bridge:  ${LSP_BRIDGE_DIR}/"
    info "  vtsls.json:  ${LANGSERVER_DIR}/vtsls.json"
    echo ""
    info "下一步：启动 Emacs，模块 init-web.el 和 init-lsp.el 将自动加载"
    echo ""
}

main "$@"
