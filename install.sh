#!/bin/zsh

# 进入克隆的文件路径
cd skemacs
# 拉取spacemacs配置
git submodule update --init --recursive
# 拷贝到配置目录
cp .spacemacs ~/
# Emacs配置目录
mkdir ~/.emacs.d
cp spacemacs ~/.emacs.d
