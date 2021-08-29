#include "spartan_data_Complex.h"
#include <complex.h> // Requires C99

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_log(JNIEnv *env, jclass clazz, jdouble x1, jdouble y1, jdouble x2, jdouble y2)
{
  complex double z1 = x1 + y1 * I;
  complex double z2 = x2 + y2 * I;
  complex double result = clog(z1) / clog(z2);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_exp(JNIEnv *env, jclass clazz, jdouble x1, jdouble y1, jdouble x2, jdouble y2)
{
  complex double z1 = x1 + y1 * I;
  complex double z2 = x2 + y2 * I;
  complex double result = cexp(z2 * clog(z1));
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_cos(JNIEnv *env, jclass clazz, jdouble x, jdouble y)
{
  complex double z = x + y * I;
  complex double result = ccos(z);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_sin(JNIEnv *env, jclass clazz, jdouble x, jdouble y)
{
  complex double z = x + y * I;
  complex double result = csin(z);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_tan(JNIEnv *env, jclass clazz, jdouble x, jdouble y)
{
  complex double z = (double)x + (double)y * I;
  complex double result = ctan(z);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_acos(JNIEnv *env, jclass clazz, jdouble x, jdouble y)
{
  complex double z = (double)x + (double)y * I;
  complex double result = cacos(z);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_asin(JNIEnv *env, jclass clazz, jdouble x, jdouble y)
{
  complex double z = (double)x + (double)y * I;
  complex double result = casin(z);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}

JNIEXPORT jobject JNICALL
Java_spartan_data_Complex_atan(JNIEnv *env, jclass clazz, jdouble x, jdouble y)
{
  complex double z = (double)x + (double)y * I;
  complex double result = catan(z);
  return (*env)->NewObject(env, clazz, (*env)->GetMethodID(env, clazz, "<init>", "(DD)V"), creal(result), cimag(result));
}
