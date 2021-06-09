@echo off

set ONYX_DIR=\dev\onyx

set failed=0
for /r tests %%v in (*.onyx) do (
    echo Checking %%~pv%%~nv.onyx

    set ERRORLEVEL=0
    %ONYX_DIR%\onyx.exe -r js --use-post-mvp-features "%%v" -o "%ONYX_DIR%\tests\%%~nv.wasm"
    if %ERRORLEVEL% GEQ 1 (
        echo Failed to compile %%~nv.onyx.
        set failed=1
        goto ::continue::
    )

    set ERRORLEVEL=0
    node %ONYX_DIR%\bin\onyx-js "%ONYX_DIR%\tests\%%~nv.wasm" > %ONYX_DIR%\tmpoutput
    if %ERRORLEVEL% GEQ 1 (
        echo Failed to run %%~nv.onyx.
        set failed=1
        goto ::continue::
    )

    set ERRORLEVEL=0
    fc %ONYX_DIR%\tmpoutput "%%~pv%%~nv" > nul
    if %ERRORLEVEL% GEQ 1 (
        echo Output did not match for %%~nv.onyx.
        set failed=1
        goto ::continue::
    )

::continue::
    del %ONYX_DIR%\tests\%%~nv.wasm > nul
)

del %ONYX_DIR%\tmpoutput

if %failed% NEQ 0 (
    echo Some of the test cases failed.
) else (
    echo All test cases passed.
)