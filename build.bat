@echo off
cls
".nuget/nuget.exe" "install" "FAKE" "-OutputDirectory" "tools" "-ExcludeVersion" "-Prerelease"
".nuget/nuget.exe" "install" "NUnit.Runners" "-OutputDirectory" "tools" "-ExcludeVersion"
"tools/FAKE/tools/Fake.exe" "build.fsx" %*
pause

