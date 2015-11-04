@echo off
echo Cleaning Up Files...
del *.local /F /S /Q
del *.identcache /F /S /Q
del *.res /F /S /Q

echo Cleaning History...
rd __history /S /Q

echo Cleaning Output...
rd Win32 /S /Q
rd Win64 /S /Q

echo Cleanup Complete.
pause
