
; Test that the symbol cache for interned symbols is purged of unreferenced symbols
; The test generates many symbols and interns them.
; Run this test with the run-symbol-cache-test.bat script.
; This will create a JFC (Java Flight Recorder) record result.jfc that can be
; opened with JMC (Java Mission Control) to examine the memory usage and GC events.

(let [(i 0)]
  (while (< i 200000)
    (symbol-intern (gensym))
    (inc! i)))
