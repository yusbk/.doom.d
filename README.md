# .doom.d
Setup for Doom Emacs

# Update
Can read directly https://github.com/hlissner/doom-emacs#install

Basically it's:

`.emacs.d/bin/doom sync` or
`.emacs.d/bin/doom upgrade`

# Run Emacs daemon on startup in Windows
- Need to create a batch file
- Win+R to open run command
- type `shell:startup`
- Create the batch file something like:

``` sh
rem Sets HOME for current shell
rem %APPDATA% is where C:\Users\<username>\AppData\Roaming is
set HOME=%HOME%

rem Clean previous server file info first
del /q ""%HOME%\\.emacs.d\\server\\*""

rem Start the Emacs daemon/server with HOME as the default directory
C:\Users\ybka\scoop\apps\emacs\current\bin\runemacs.exe --daemon
```

- Better explanation to be found [here](https://emacs.stackexchange.com/a/69793/10811 "StackExchange")
