GCC = gcc

# Name of the shared library file to be created
LIB = libspartan.dll

# Location of JDK include files
JDK_INC = "C:\jdk-22.0.2\include"

# Location of Win32 JDK include files
JDK_INC_WIN32 = "C:\jdk-22.0.2\include\win32"

# Compile JNI libraries
$(LIB): spartan_data_Complex.h spartan_data_Complex.c
	$(GCC) -W -Wall -m64 -g -shared -o $(LIB) -I $(JDK_INC) -I $(JDK_INC_WIN32) spartan_data_Complex.c
