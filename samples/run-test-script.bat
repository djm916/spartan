@ECHO OFF
REM Pass the name of the JFR output file as the first command line argument
REM Pass the name of the Spartan test script as the second command line argument
java -XX:StartFlightRecording:filename=%1 -Dspartan.debug-logging="true" -jar ./Spartan.jar %2
