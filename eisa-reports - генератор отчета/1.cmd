@Echo Off

Rem (c) СКБ ПСИС, 2014-2018

Echo Cкрипт удаления временных файлов Deplhi 
Echo ---

Rem Запрос подтверждения

Rem Set /p strAnswer="Удалить временные файлы [Y(да)]?"
Rem If /i "%strAnswer%" EQU "y" Goto DoContinue
Rem If /i "%strAnswer%" NEQ "д" Goto Done 
Rem Echo Текущий каталог - %CD%

Rem Очищаем каталог исходников

Echo Удаляем временные файлы в каталоге исходного кода
For /R %%i In (C) Do (
  If Exist "%%~dpi*.~*" (
    Echo Удаляем "%%~dpi*.~*"
    Del "%%~dpi*.~*"
  )
  If Exist "%%~dpi*.bak" (
    Echo Удаляем "%%~dpi*.bak"
    Del "%%~dpi*.bak"
  )
  If Exist "%%~dpi*.ddp" (
    Echo Удаляем "%%~dpi*.ddp"
    Del "%%~dpi*.ddp"
  )
  If Exist "%%~dpi*.dcu" (
    Echo Удаляем "%%~dpi*.dcu"
    Del "%%~dpi*.dcu"
  )
  If Exist "%%~dpi*.cbk" (
    Echo Удаляем "%%~dpi*.cbk"
    Del "%%~dpi*.cbk"
  )
) 

Echo Завершение
