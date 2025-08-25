# Uniform Server - 免费轻量级WAMP服务器解决方案

Uniform Server is a free lightweight WAMP server solution for Windows.
Build using a modular design approach, it includes the latest versions of Apache, MySQL or MariaDB, PHP (with version switching), phpMyAdmin or Adminer.

No installation required! No registry dust! Just unpack and fire up!

## 🌟 项目概述

Uniform Server 是一个**免费、轻量级的WAMP服务器解决方案**，专为Windows平台设计。它采用模块化设计理念，包含了最新版本的Apache、MySQL或MariaDB、PHP（支持版本切换）、phpMyAdmin或Adminer等Web开发必需组件。

### 🎯 核心特性
- ✅ **零安装**：无需安装，无注册表污染，解压即用
- ✅ **模块化设计**：按需安装组件，避免资源浪费
- ✅ **完整WAMP环境**：Apache + MySQL + PHP + 管理工具
- ✅ **多版本支持**：PHP 7.0-8.3版本动态切换
- ✅ **图形化管理**：直观的GUI控制界面
- ✅ **完全中文化**：100%中文界面，适合中文用户

---

## 🖥️ UniController - 核心控制器

The UniController is the heart of the Uniform Server Package where everything can be controlled, modified and updated. It is built using Pascal and compiled with Lazarus.

**UniController** 是 Uniform Server 包的核心，在这里可以控制、修改和更新所有功能。它使用 Pascal 语言构建，并通过 Lazarus 编译。

### 🔧 技术架构
- **开发语言**: Pascal (Object Pascal)
- **开发环境**: Lazarus IDE
- **编译器**: Free Pascal Compiler (FPC)
- **界面库**: Lazarus Component Library (LCL)
- **网络库**: Synapse TCP/IP 库

---

## 🚀 创建开发环境

Create a working environment for compiling and testing code:

创建用于编译和测试代码的开发环境：

### 📁 环境搭建步骤

 1. **创建新文件夹**: 创建一个新文件夹，例如 `z_controller`
 2. **下载最新版本**: 下载最新版本的 Uniform Server ZeroXV，例如 `15_x_x_ZeroXV.exe` 并保存到 `z_controller` 文件夹
 3. **解压安装包**: 双击下载的文件 (`15_x_x_ZeroXV.exe`)；这将把 Uniform Server ZeroXV 解压到 `z_controller\UniServerZ` 文件夹
 4. **下载源代码**: 从 Github 下载源代码到 `z_controller\UniServerZ` 文件夹
 5. **注意**: 会创建两个新文件夹：`z_controller\UniServerZ\synapse` 和 `z_controller\UniServerZ\unicon_images`
    项目源代码会添加到 `z_controller\UniServerZ` 文件夹
 6. **完成**: 开发环境创建完成

### 📋 目录结构说明
```
z_controller/
├── UniServerZ/                    # 主项目目录
│   ├── UniController/             # 控制器源代码
│   ├── synapse/                   # TCP/IP网络通信库
│   ├── unicon_images/            # 界面图标资源
│   ├── core/                      # 核心服务器组件
│   ├── home/                      # 应用程序和配置
│   └── README.md                  # 项目说明文档
└── 15_x_x_ZeroXV.exe             # 安装包
```

---

## 🔨 编译 UniController

With the working environment in place, you are ready to compile UniController as follows:

开发环境就绪后，您可以按以下步骤编译 UniController：

### 🛠️ 编译步骤

 1. **启动 Lazarus**: 启动 Lazarus IDE
 2. **关闭现有项目**: 项目 > 关闭项目
 3. **打开项目**: 在弹出窗口中，点击"打开项目"按钮
    导航到文件夹：`z_controller\UniServerZ`
    点击文件 `UniController.lpi`，点击打开按钮
    项目打开并准备编译
 4. **快速测试运行**: 运行 > 运行 或按 F9
    注意：Synapse 会产生几个警告提示消息；这些不是错误
    最后一行显示：项目 "UniController" 成功构建
 5. **运行控制器**: UniController 将运行

 现在您可以按需更改代码并重新编译。

### ⚡ 快速编译命令
```bash
# 切换到项目目录
cd "D:\UO\work\z_controller\UniServerZ\UniController"

# Release模式编译
& "C:\lazarus\lazbuild.exe" --lazarusdir="C:\lazarus" --compiler="C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe" --build-mode=Release UniController.lpi

# 强制重新编译
& "C:\lazarus\lazbuild.exe" --lazarusdir="C:\lazarus" --compiler="C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe" --build-mode=Release --force-rebuild UniController.lpi
```

---

## 🌍 中文化状态

### ✅ 已完成的中文化内容
- **主界面菜单**: 100% 中文化完成
- **弹窗表单**: 100% 中文化完成
- **消息对话框**: 100% 中文化完成
- **帮助信息**: 100% 中文化完成
- **错误提示**: 100% 中文化完成

### 🔧 中文化技术实现
- **翻译文件**: 使用GNU gettext PO格式
- **实现方式**: Lazarus国际化框架 + 直接赋值
- **安全检查**: 添加Assigned()检查防止访问违规

---

## 📊 编译环境要求

### 🖥️ 开发环境
- **Lazarus IDE**: 4.2版本或更高
- **Free Pascal编译器**: 3.2.2版本或更高
- **目标平台**: x86_64-win64
- **构建模式**: Release模式

### 🔧 系统要求
- **操作系统**: Windows 7/8/10/11 (64位)
- **内存**: 最少2GB，推荐4GB以上
- **磁盘空间**: 最少1GB可用空间

---

## 🚀 开发工作流

### 1. **代码修改**
- 在 Lazarus IDE 中编辑源代码
- 使用内置调试器进行调试
- 实时预览界面变化

### 2. **编译测试**
- 按 F9 快速编译和运行
- 检查编译警告和错误
- 验证功能正常运行

### 3. **部署更新**
- 编译成功后复制到项目根目录
- 测试新功能
- 更新文档和说明

---

## 🔍 故障排除

### 常见编译问题

#### 1. 路径问题
**问题**: 找不到编译器或库文件
**解决**: 检查 Lazarus 和 FPC 安装路径

#### 2. 依赖问题
**问题**: 缺少必要的库文件
**解决**: 确保 Synapse 和 unicon_images 文件夹存在

#### 3. 权限问题
**问题**: 无法写入输出文件
**解决**: 以管理员身份运行 Lazarus

### 编译优化建议
1. **使用 Release 模式**: 获得最佳性能
2. **清理中间文件**: 定期清理编译缓存
3. **检查依赖**: 确保所有必要的库都已安装

---

## 📚 相关资源

### 开发资源
- **Lazarus IDE**: https://www.lazarus-ide.org/
- **Free Pascal**: https://www.freepascal.org/
- **Synapse 库**: 内置在项目中

### 学习资源
- **Pascal 教程**: Free Pascal 官方文档
- **Lazarus 指南**: Lazarus IDE 使用教程
- **LCL 组件**: Lazarus Component Library 参考

---

## 🏆 项目特色

### 技术优势
- **跨平台兼容**: 使用 Lazarus 和 FPC 实现跨平台支持
- **模块化设计**: 清晰的代码结构和模块分离
- **性能优化**: Release 模式编译获得最佳性能
- **调试友好**: 内置调试器和错误检查

### 用户体验
- **零配置启动**: 开箱即用的开发环境
- **实时编译**: 快速编译和测试循环
- **中文友好**: 100%中文化界面
- **文档完善**: 详细的使用说明和故障排除

---

## 📞 技术支持

### 问题反馈
- **编译问题**: 检查环境配置和依赖
- **功能问题**: 查看源代码和注释
- **中文化问题**: 检查翻译文件加载

### 贡献方式
- **代码贡献**: 欢迎提交改进代码
- **文档贡献**: 帮助完善中文文档
- **测试反馈**: 报告Bug和兼容性问题

---

*本文档基于项目源码、配置文件、文档等资料的深度分析生成*  
*项目状态: 完全中文化 ✅ 编译成功 ✅ 文档完善 ✅*  
*文档生成时间: 2025年8月25日*  
*维护者: 汉化团队 - 鸦鸦 (Yaya)*
