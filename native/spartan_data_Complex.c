#include "spartan_data_Complex.h"
#include <complex.h> // Requires C99

JNIEXPORT jobject JNICALL Java_spartan_data_Complex_log
  (JNIEnv *env, jclass clazz, jdouble x1, jdouble y1, jdouble x2, jdouble y2)
{
  complex double z1 = x1 + y1 * I; // z1 = base
  complex double z2 = x2 + y2 * I; // z2 = argument
  complex double z = clog(z1) / clog(z2); // z = result
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(z), cimag(z));
}

JNIEXPORT jobject JNICALL Java_spartan_data_Complex_exp
  (JNIEnv *env, jclass clazz, jdouble x1, jdouble y1, jdouble x2, jdouble y2)
{
  complex double z1 = x1 + y1 * I;
  complex double z2 = x2 + y2 * I;
  complex double z = cexp(z2 * clog(z1));
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(z), cimag(z));
}

