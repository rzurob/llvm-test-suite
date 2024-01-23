!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 4, 2003
!*
!*  DESCRIPTION                : Testing the values of the constants
!*                               provided by the ISO_C_BINDING module.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/04/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      @PROCESS ESCAPE
      program iccons
        use iso_c_binding
        if (C_SIGNED_CHAR /= 1) error stop 1
        if (C_SHORT /= 2) error stop 2
        if (C_INT /= 4) error stop 3
#ifdef BITMODE64
        if (C_LONG /= 8) error stop 4
#else
        if (C_LONG /= 4) error stop 4
#endif
        if (C_LONG_LONG /= 8) error stop 5
#ifdef BITMODE64
        if (C_SIZE_T /= 8) error stop 6
        if (C_INTPTR_T /= 8) error stop 7
#else
        if (C_SIZE_T /= 4) error stop 6
        if (C_INTPTR_T /= 4) error stop 7
#endif
        if (C_INT8_T /= 1) error stop 8
        if (C_INT16_T /= 2) error stop 9
        if (C_INT32_T /= 4) error stop 10
        if (C_INT64_T /= 8) error stop 11
        if (C_INT_LEAST8_T /= 1) error stop 12
        if (C_INT_LEAST16_T /= 2) error stop 13
        if (C_INT_LEAST32_T /= 4) error stop 14
        if (C_INT_LEAST64_T /= 8) error stop 15
        if (C_INT_FAST8_T /= 1) error stop 16
#ifdef __linux__
#ifdef BITMODE64
        if (C_INT_FAST16_T /= 8) error stop 17
#else
        if (C_INT_FAST16_T /= 4) error stop 17
#endif
#else
        if (C_INT_FAST16_T /= 2) error stop 17
#endif
#if (defined(__linux__) && defined(BITMODE64))
        if (C_INT_FAST32_T /= 8) error stop 18
#else
        if (C_INT_FAST32_T /= 4) error stop 18
#endif
        if (C_INT_FAST64_T /= 8) error stop 19
        if (C_INTMAX_T /= 8) error stop 20
        if (C_FLOAT /= 4) error stop 21
        if (C_DOUBLE /= 8) error stop 22
        if (C_LONG_DOUBLE /= 16) error stop 23
        if (C_FLOAT_COMPLEX /= 4) error stop 24
        if (C_DOUBLE_COMPLEX /= 8) error stop 25
        if (C_LONG_DOUBLE_COMPLEX /= 16) error stop 26
#if (defined(__APPLE__))
        if (C_BOOL /= 4) error stop 27
#else
        if (C_BOOL /= 1) error stop 27
#endif
        if (C_CHAR /= 1) error stop 28
        if (C_NULL_CHAR /= '\0') error stop 29
        if (C_ALERT /= ACHAR(7)) error stop 30
        if (C_BACKSPACE /= '\b') error stop 31
        if (C_FORM_FEED /= '\f') error stop 32
        if (C_NEW_LINE /= '\n') error stop 33
        if (C_CARRIAGE_RETURN /= ACHAR(13)) error stop 34
        if (C_HORIZONTAL_TAB /= '\t') error stop 35
        if (C_VERTICAL_TAB /= ACHAR(11)) error stop 36
      end program iccons
