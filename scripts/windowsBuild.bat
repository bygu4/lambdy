@echo off

for /r %%i in (*.sln) do (
    dotnet build %%i --nologo -clp:NoSummary -v:m
    if errorlevel 1 (
        echo - %%~nxi: build failed
        exit 1
    ) else (
        dotnet test %%i --no-build --nologo -v:m
        if errorlevel 1 (
            echo - %%~nxi: test failed
            exit 1
        ) else (
            echo - %%~nxi: test passed
        )
    )
)
