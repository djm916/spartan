@ECHO OFF
REM Startup the interpreter on Windows systems
REM Launches Java on the executable .jar, passing through all command-line arguments
java -Dspartan.debug="true" --enable-preview -jar Spartan.jar %*
