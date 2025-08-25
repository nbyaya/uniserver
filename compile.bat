@echo off
echo 开始编译 Uniform Server 项目...
echo.

REM 设置编译环境变量
set LAZARUS_DIR=C:\lazarus
set FPC_DIR=C:\lazarus\fpc\3.2.2\bin\x86_64-win64

REM 检查Lazarus是否安装
if not exist "%LAZARUS_DIR%" (
    echo 错误: 未找到Lazarus安装目录: %LAZARUS_DIR%
    echo 请检查Lazarus是否正确安装
    pause
    exit /b 1
)

REM 检查FPC是否安装
if not exist "%FPC_DIR%\fpc.exe" (
    echo 错误: 未找到FPC编译器: %FPC_DIR%\fpc.exe
    echo 请检查Free Pascal Compiler是否正确安装
    pause
    exit /b 1
)

echo 找到编译环境:
echo   Lazarus: %LAZARUS_DIR%
echo   FPC: %FPC_DIR%
echo.

REM 创建Bin目录结构
if not exist "Bin" mkdir Bin
if not exist "Bin\lib" mkdir Bin\lib
if not exist "Bin\lib\x86_64-win64" mkdir Bin\lib\x86_64-win64

echo 编译输出目录: Bin\
echo.

REM 编译UniController
echo 正在编译 UniController...
cd UniController
"%LAZARUS_DIR%\lazbuild.exe" --lazarusdir="%LAZARUS_DIR%" --compiler="%FPC_DIR%\fpc.exe" --build-mode=Release UniController.lpi
if %ERRORLEVEL% neq 0 (
    echo 错误: UniController 编译失败
    pause
    exit /b 1
)
cd ..

echo.
REM 编译UniService
echo 正在编译 UniService...
cd UniService
"%LAZARUS_DIR%\lazbuild.exe" --lazarusdir="%LAZARUS_DIR%" --compiler="%FPC_DIR%\fpc.exe" --build-mode=Release UniService.lpi
if %ERRORLEVEL% neq 0 (
    echo 警告: UniService 编译失败，但继续执行
)
cd ..

echo.
echo 编译完成！
echo 编译输出文件位于 Bin\ 目录下
echo.

REM 显示编译结果
echo 编译输出文件:
dir /s Bin\ 2>nul
echo.

pause
