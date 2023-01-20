AC_INIT([rcontroll],[0.1.0],[sylvain.m.schmitt@gmail.com])
AC_MSG_NOTICE([rcontroll: 0.1.0])

## Ensure TMPDIR is set.
AC_MSG_CHECKING([whether we have a suitable tempdir])
TMPDIR=$("${R_HOME}/bin/R" --vanilla --slave -e "cat(dirname(tempdir()))")
AC_MSG_RESULT([${TMPDIR}])

# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_PROG_CC([gcc cc])
AC_PROG_CXX([g++ c++])

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

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    c++:       ${CXX}
    c++ flags: ${CXXFLAGS} ${CPPFLAGS}
    gsl-flags: ${GSL_CFLAGS}
    gsl-libs:  ${GSL_LIBS}

  --------------------------------------------------
"