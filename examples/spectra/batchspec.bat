for /f %%i in ('dir *.out /b') do (
Multiwfn %%i < UV-Vis.txt > NUL
rename DISLIN.png %%~ni.png
)
