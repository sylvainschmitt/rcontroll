define([pkgversion], esyscmd([sh -c "grep Version: DESCRIPTION | cut -d' ' -f2 | tr -d '\n'"]))
AC_INIT([rcontroll],[pkgversion],[sylvain.m.schmitt@gmail.com])
AC_MSG_NOTICE([${PACKAGE_NAME}: ${PACKAGE_VERSION}])


## We are using C++
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CXX

## Ensure TMPDIR is set.
AC_MSG_CHECKING([whether we have a suitable tempdir])
TMPDIR=$("${R_HOME}/bin/R" --vanilla --slave -e "cat(dirname(tempdir()))")
AC_MSG_RESULT([${TMPDIR}])



## Use gsl-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: gsl-config(1)
AC_PATH_PROG([GSL_CONFIG], [gsl-config])
## If gsl-config was found, let's use it
if test "${GSL_CONFIG}" != ""; then
    # Use gsl-config for header and linker arguments
    GSL_CFLAGS=`${GSL_CONFIG} --cflags`
    GSL_LIBS=`${GSL_CONFIG} --libs`
else
    AC_MSG_FAILURE([
  ---------------------------------------------
  
    gsl-config not found, is GSL installed?
    
   'GSL' and its header files are required.
   Please install:
   * libgsl-dev (on Debian and Ubuntu)
   * ibgsl-devel (on Fedora, CentOS and RHEL)
   * libgsl (via Homebrew on macOS)
   * libgsl (on Solaris)
   and try again.
   If you believe this library is installed on your system but
   this script is simply unable to find it, you can specify the
   include and lib paths manually:
   R CMD INSTALL ${PACKAGE_NAME} \\
     --configure-vars='LIBS=-L/path/to/libs CPPFLAGS=-I/path/to/headers'
  ---------------------------------------------])
fi

AC_MSG_CHECKING([if GSL version >= 2.5])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <string.h>
#include <gsl/gsl_version.h>
#include <stdio.h>
#include <stdlib.h>
int main() {
#ifdef GSL_VERSION
   int major, minor;
   char *gslv = GSL_VERSION;
   if ((sscanf(gslv, "%d.%d", &major, &minor)) != 2) {
     exit (1);
   }
   exit (minor < 5);
   exit (major < 2);
#else
  exit(1);
#endif
}
]])],
[gsl_version_ok=yes],
[gsl_version_ok=no],
[gsl_version_ok=yes])
if test "${gsl_version_ok}" = no; then
  AC_MSG_ERROR([Need GSL version >= 1.16])
else
  AC_MSG_RESULT([yes])
fi

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    gsl-flags: ${GSL_CFLAGS}
    gsl-libs:  ${GSL_LIBS}

  --------------------------------------------------
"