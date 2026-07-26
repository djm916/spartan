@ECHO OFF
REM Executes the symbol cache test
REM Pass the name of the JFR output file as the first command line argument
java -XX:StartFlightRecording:filename=%1 -Dspartan.debug-logging="true" -jar ./Spartan.jar ./samples/symbol-cache-test.s
