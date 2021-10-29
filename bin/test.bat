@echo off

set ONYX_DIR=\dev\onyx

set failed=0
for /r tests %%v in (*.onyx) do (
    echo Checking %%~pv%%~nv.onyx

    set ERRORLEVEL=0
    %ONYX_DIR%\onyx.exe run "%%v" > %ONYX_DIR%\tmpoutput
    if %ERRORLEVEL% GEQ 1 (
        echo Failed to compile %%~nv.onyx.
        set failed=1
        goto continue
    )

    set ERRORLEVEL=0
    fc %ONYX_DIR%\tmpoutput "%%~pv%%~nv" > nul
    if %ERRORLEVEL% GEQ 1 (
        echo Output did not match for %%~nv.onyx.
        set failed=1
    )

::continue::
    del %ONYX_DIR%\tmpoutput
)

del %ONYX_DIR%\tmpoutput

if %failed% NEQ 0 (
    echo Some of the test cases failed.
) else (
    echo All test cases passed.
)