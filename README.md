# Spartan

Spartan is an experimental Lisp dialect inspired by languages such as Scheme, Common Lisp, and Clojure. It was created and developed primarily for the author's own education. It includes a bytecode compiler and interpreter implemented in Java.

Some of Spartan's major features include: a homoiconic expression-based syntax, first-class anonymous procedures, lexical closures, proper tail recursion, (non-hygenic) macros, first-class continuations, namespaces, user-defined record types, and pattern matching.

## Building the Project (Windows)

Building on Linux systems is currently not supported; the project can only be built on Windows.

### Prerequisites

To build the project, the following dependencies must first be installed on your development machine:

* Java SDK version 22 or higher
* A C environment (e.g., MSYS2) including a C99 compiler and the "make" utility
* Ant
* JUnit

To build on your system, you will need to modify:

* build.xml, such that the project's "classpath" property includes the full paths to all required Java .jar dependencies.
* native/Makefile, such that the JDK_INC and JDK_INC_WIN32 refer to Java's JNI include files directory

To compile the project use:

```
ant compile
```

This will generate the primary artifacts required to run Spartan:

* Spartan.jar - an executable JAR containing the implementation of Spartan CLI
* libspartan.dll - the supporting C library

To generate Javadoc use:

```
ant javadoc
```

Javadoc artifacts are located in the "javadoc" directory.

To run the tests use:

```
ant test
```

After running the tests, the results will be found in the directory "test-results".

To delete all build artifacts, use:

```
ant clean
```

## Usage

Spartan can be invoked directly via Java:

```
java -jar Spartan.jar
```

For convenience, invoke Spartan using the script:

```
spartan.bat
```

You can also invoke Spartan in "debug mode" using the script:

```
spartan-debug.bat
```

## Configuration

Spartan has the following configuration options, set via Java system properties:

**spartan.optimize-tail-calls**

Enables or disables optimization of tail calls (e.g., the removal of call frames for calls in tail position). It can be useful to disable this optimization when debugging. The default is "true". 

**spartan.debug-logging**

Enables or disables debug logging. The default is "false". When enabled ("true"), the following options are also recognized:

**spartan.emit-bytecode**

Enables or disables the emission of bytecode listings during compilation. Default is "true".

**spartan.show-macro-expansion**

Enables or disables the logging of macro expansions during compilation. Default is "true".

The environment variable **SPARTANPATH**, if set, must contain a list of directories separated by a (system-specific) delimiter. This list of directories will be searched, in order, when Spartan needs to load a source file. If not set, the default value is the current working directory.

The environment variable **SPARTANHOME**, if set, should be set to Spartan's installation directory. This should be set once when installing Spartan on a given system. 

## Contributing

As this project is intended for the author's own education, no pull requests will be accepted at this time.

## License
No current license.