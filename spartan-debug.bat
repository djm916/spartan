@ECHO OFF
REM Startup the interpreter on Windows systems
REM Launches Java on the executable .jar, passing through all command-line arguments
java -Dspartan.debug-logging="true" -Dspartan.emit-bytecode="true" -Dspartan.show-macro-expansion="true" -Dspartan.optimize-tail-calls="false" -ea --enable-preview -jar Spartan.jar %*
