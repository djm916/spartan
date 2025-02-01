# Spartan

Spartan is an experimental Lisp dialect inspired by languages such as Scheme, Common Lisp, and Clojure. It was created and developed primarily for the author's own education. It includes a bytecode compiler and interpreter implemented in Java.

Some of Spartan's major features include: a homoiconic expression-based syntax, first-class anonymous procedures, lexical closures, proper tail recursion, (non-hygenic) macros, first-class continuations, namespaces, user-defined record types, and pattern matching.

## Building the Project (Windows)

Building on Linux systems is currently not supported; the project can only be built on Windows.

### Prerequisites

To build the project, the following dependencies must first be installed on your development machine:

* Java SDK version 22 or higher
* A C environment (e.g., MSYS2) including a compiler and "make"
* Ant
* JUnit

This project uses the Ant build system for Java. To build on your system, you will need to modify the build.xml file in the root directory such that the project's "classpath" property includes the full paths to all required Java .jar dependencies.

To compile the project use:

```
ant compile
```

This will generate the executable JAR, Spartan.jar.

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

## Contributing

As this project is intended for the author's own education, no pull requests will be accepted at this time.

## License
No current license.