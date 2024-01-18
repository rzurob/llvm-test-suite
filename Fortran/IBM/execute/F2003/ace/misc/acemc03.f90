!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc03
!*
!*  DATE                       : 2006-07-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values from C functions (complex)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Interoperability, C
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  In AC, invoke C functions returning various types of complex numbers, and
!*  verify their correctness.  When comparing floating point numbers, we need to
!*  follow a roundabout route:
!*  1. check if they are both NaNs (two NaNs are never equal, by definition,
!*     so we have to verify that they're NaNs separately; if they are, then
!*     the comparison succeeds)
!*  2. check if they are exactly equal (approximate comparisons test if two
!*     numbers are in the same range by adding or subtracting a small number
!*     and comparing the result; adding even the smallest value to Inf yields
!*     NaN, causing failure; Inf is unique, so we can and must test for it
!*     exactly)
!*  3. check if they are approximately equal.
!*  Of course, this has to be applied to both the real and the imaginary parts.
!*  All of this happens in the various diff routines (diff4, diff8, diff16).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc03

  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface

     integer(C_INT) function cfloat_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function cfloat_size

     integer(C_INT) function cdbl_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function cdbl_size

     integer(C_INT) function cldbl_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function cldbl_size


     function float_complex_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       complex(C_FLOAT_COMPLEX) :: float_complex_fun
       integer(C_INT), value :: sel
     end function float_complex_fun

     function double_complex_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       complex(C_DOUBLE_COMPLEX) :: double_complex_fun
       integer(C_INT), value :: sel
     end function double_complex_fun

     function long_double_complex_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       complex(C_LONG_DOUBLE_COMPLEX) :: long_double_complex_fun
       integer(C_INT), value :: sel
     end function long_double_complex_fun

  end interface

  complex(C_FLOAT_COMPLEX) :: val

  complex (C_LONG_DOUBLE_COMPLEX) :: ldarr(5), ldarr2(5), ldarrExp(5)
  complex (C_DOUBLE_COMPLEX) ::      darr(5), darr2(5), darrExp(5)
  complex (C_FLOAT_COMPLEX) ::       farr(5), farr2(5), farrExp(5)

  integer(4) :: errorCount

  integer(C_INT) :: i


  if (cfloat_size() /= C_FLOAT_COMPLEX .or. cdbl_size() /= C_DOUBLE_COMPLEX .or. cldbl_size() /= C_LONG_DOUBLE_COMPLEX) then
     print *, "Incorrect float/double/long double size:"
     print *, " float complex size is ", cfloat_size(), " should be ", C_FLOAT_COMPLEX
     print *, " double complex size is ", cdbl_size(), " should be ", C_DOUBLE_COMPLEX
     print *, " long double complex size is ", cldbl_size(), " should be ", C_LONG_DOUBLE_COMPLEX
     error stop 20_4
  end if


  errorCount = 0


  ldarr    = (/ (long_double_complex_fun(i),i=0,4) /)
  ldarr2   = (/ long_double_complex_fun(0), &
                long_double_complex_fun(1), &
                long_double_complex_fun(2), &
                long_double_complex_fun(3), &
                long_double_complex_fun(4) /)
  ldarrExp = (/ (1e-9_C_LONG_DOUBLE,-1e-9_C_LONG_DOUBLE), &
                (-1e-37_C_LONG_DOUBLE,1e-37_C_LONG_DOUBLE), &
                (-1e37_C_LONG_DOUBLE,-1e37_C_LONG_DOUBLE), &
                (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
                (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)) /)
  do i=1,5
     if (diff16(ldarr(i),ldarr2(i),ldarrExp(i),'no spec',i)) errorCount = errorCount + 1
  end do


  ldarr    = (/ complex(C_LONG_DOUBLE_COMPLEX):: (long_double_complex_fun(i),i=0,4) /)
  ldarr2   = (/ complex(C_LONG_DOUBLE_COMPLEX):: &
                long_double_complex_fun(0), &
                long_double_complex_fun(1), &
                long_double_complex_fun(2), &
                long_double_complex_fun(3), &
                long_double_complex_fun(4) /)
  ldarrExp = (/ complex(C_LONG_DOUBLE_COMPLEX):: &
                (1e-9_C_LONG_DOUBLE,-1e-9_C_LONG_DOUBLE), &
                (-1e-37_C_LONG_DOUBLE,1e-37_C_LONG_DOUBLE), &
                (-1e37_C_LONG_DOUBLE,-1e37_C_LONG_DOUBLE), &
                (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
                (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)) /)
  do i=1,5
     if (diff16(ldarr(i),ldarr2(i),ldarrExp(i),'type spec',i)) errorCount = errorCount + 1
  end do


  darr    = (/ (double_complex_fun(i),i=0,4) /)
  darr2   = (/ double_complex_fun(0), &
               double_complex_fun(1), &
               double_complex_fun(2), &
               double_complex_fun(3), &
               double_complex_fun(4) /)
  darrExp = (/ (1e-9_C_DOUBLE,-1e-9_C_DOUBLE), &
               (-1e-37_C_DOUBLE,1e-37_C_DOUBLE), &
               (-1e37_C_DOUBLE,-1e37_C_DOUBLE), &
               (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
               (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)) /)

  do i=1,5
     if (diff8(darr(i),darr2(i),darrExp(i),'no spec',i)) errorCount = errorCount + 1
  end do


  darr    = (/ complex(C_DOUBLE_COMPLEX):: (double_complex_fun(i),i=0,4) /)
  darr2   = (/ complex(C_DOUBLE_COMPLEX):: &
               double_complex_fun(0), &
               double_complex_fun(1), &
               double_complex_fun(2), &
               double_complex_fun(3), &
               double_complex_fun(4) /)
  darrExp = (/ complex(C_DOUBLE_COMPLEX):: &
               (1e-9_C_DOUBLE,-1e-9_C_DOUBLE), &
               (-1e-37_C_DOUBLE,1e-37_C_DOUBLE), &
               (-1e37_C_DOUBLE,-1e37_C_DOUBLE), &
               (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
               (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)) /)

  do i=1,5
     if (diff8(darr(i),darr2(i),darrExp(i),'type spec',i)) errorCount = errorCount + 1
  end do


  farr    = (/ (float_complex_fun(i),i=0,4) /)
  farr2   = (/ float_complex_fun(0), &
               float_complex_fun(1), &
               float_complex_fun(2), &
               float_complex_fun(3), &
               float_complex_fun(4) /)
  farrExp = (/ (1e-5_C_FLOAT,-1e-5_C_FLOAT), &
               (-1e-37_C_FLOAT,1e-37_C_FLOAT), &
               (-1e37_C_FLOAT,-1e37_C_FLOAT), &
               (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
               (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)) /)

  do i=1,5
     if (diff4(farr(i),farr2(i),farrExp(i), 'no spec', i)) errorCount = errorCount + 1
  end do


  farr    = (/ complex(C_FLOAT_COMPLEX):: (float_complex_fun(i),i=0,4) /)
  farr2   = (/ complex(C_FLOAT_COMPLEX):: &
               float_complex_fun(0), &
               float_complex_fun(1), &
               float_complex_fun(2), &
               float_complex_fun(3), &
               float_complex_fun(4) /)
  farrExp = (/ complex(C_FLOAT_COMPLEX):: &
               (1e-5_C_FLOAT,-1e-5_C_FLOAT), &
               (-1e-37_C_FLOAT,1e-37_C_FLOAT), &
               (-1e37_C_FLOAT,-1e37_C_FLOAT), &
               (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
               (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)) /)

  do i=1,5
     if (diff4(farr(i),farr2(i),farrExp(i), 'type spec', i)) errorCount = errorCount + 1
  end do


  if (errorCount > 0) call zzrc(errorCount)

contains

  logical function diff16(v1, v2, exp, txt, id)
    complex(16), intent(in) :: v1, v2, exp
    character(*) :: txt
    integer :: id
    logical(4) :: precision_x32, all_nan
    ! check first for NaN:
    ! all_nan = (v1 .ne. v1 .and. v2 .ne. v2 .and. exp .ne. exp)
    all_nan = ((ieee_is_nan(real(v1)) .or. ieee_is_nan(aimag(v1))) &
         .and. (ieee_is_nan(real(v2)) .or. ieee_is_nan(aimag(v2))) &
         .and. (ieee_is_nan(real(exp)).or. ieee_is_nan(aimag(exp))))
    diff16 = .not. all_nan .and. .not. (precision_x32(v1,exp) .and. precision_x32(v2,exp))
    if (diff16) then
        print *, "Long double complex mismatch / ", txt, id
        print *, "  ",v1
        print *, "  ",v2
        PRINT *, "should be"
        print *, "  ",exp
    end if
    return
  end function diff16

  logical function diff8(v1, v2, exp, txt, id)
    complex(8), intent(in) :: v1, v2, exp
    character(*) :: txt
    integer :: id
    logical(4) :: precision_x16, all_nan
    ! check first for NaN:
    ! all_nan = (v1 .ne. v1 .and. v2 .ne. v2 .and. exp .ne. exp)
    all_nan = ((ieee_is_nan(real(v1)) .or. ieee_is_nan(aimag(v1))) &
         .and. (ieee_is_nan(real(v2)) .or. ieee_is_nan(aimag(v2))) &
         .and. (ieee_is_nan(real(exp)).or. ieee_is_nan(aimag(exp))))
    diff8 = .not. all_nan .and. .not. (precision_x16(v1,exp) .and. precision_x16(v2,exp))
    if (diff8) then
        print *, "Double complex mismatch / ", txt, id
        print *, "  ",v1
        print *, "  ",v2
        PRINT *, "should be"
        print *, "  ",exp
    end if
    return
  end function diff8

  logical function diff4(v1, v2, exp, txt, id)
    complex(4), intent(in) :: v1, v2, exp
    character(*) :: txt
    integer :: id
    logical(4) :: precision_x8, all_nan
    ! check first for NaN:
    ! all_nan = (v1 .ne. v1 .and. v2 .ne. v2 .and. exp .ne. exp)
    all_nan = ((ieee_is_nan(real(v1)) .or. ieee_is_nan(aimag(v1))) &
         .and. (ieee_is_nan(real(v2)) .or. ieee_is_nan(aimag(v2))) &
         .and. (ieee_is_nan(real(exp)).or. ieee_is_nan(aimag(exp))))
    diff4 = .not. all_nan .and. .not. (precision_x8(v1,exp) .and. precision_x8(v2,exp))
    if (diff4) then
        print *, "Float complex mismatch / ", txt, id
        print *, "  ",v1
        print *, "  ",v2
        PRINT *, "should be"
        print *, "  ",exp
    end if
    return
  end function diff4

end program acemc03
