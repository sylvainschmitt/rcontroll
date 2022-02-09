AC_INIT([rcontroll],[0.1.0])

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

    gsl-flags: ${GSL_CFLAGS}
    gsl-libs:  ${GSL_LIBS}

  --------------------------------------------------
"