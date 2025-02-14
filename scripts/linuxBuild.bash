windows_targeted=${PWD}/.config/windowsTargeted.txt

for f in $(find . -name "*.sln"); do (
    dotnet build $f --nologo -clp:NoSummary  -v:m
    if [ $? -eq 0 ]; then
        dotnet test $f --no-build --nologo -v:m
        if [ $? -eq 0 ]; then
            echo - ${f##*/}: test passed
        else
            echo - ${f##*/}: test failed
            exit 1
        fi
    else
        echo - ${f##*/}: build failed
        exit 1
    fi
) done
