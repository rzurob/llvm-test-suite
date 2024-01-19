!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-07-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values from C functions (real)
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
!*  In AC, invoke C functions returning various types of reals/floats, and
!*  verify their correctness.  When comparing two reals/floats, we need to
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
!*  All of this happens in the various diff routines (diff4, diff8, diff16).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc02

  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface

     integer(C_INT) function float_size() bind (c)
       use, intrinsic :: iso_c_binding
     end function float_size

     integer(C_INT) function dbl_size() bind (c)
       use, intrinsic :: iso_c_binding
     end function dbl_size

     integer(C_INT) function ldbl_size() bind (c)
       use, intrinsic :: iso_c_binding
     end function ldbl_size


     function float_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       real(C_FLOAT) :: float_fun
       integer(C_INT), value :: sel
     end function float_fun

     function double_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       real(C_DOUBLE) :: double_fun
       integer(C_INT), value :: sel
     end function double_fun

     function long_double_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       real(C_LONG_DOUBLE) :: long_double_fun
       integer(C_INT), value :: sel
     end function long_double_fun

  end interface

  real (C_FLOAT) ::       farr(5), farr2(5), farrExp(5)
  real (C_DOUBLE) ::      darr(5), darr2(5), darrExp(5)
  real (C_LONG_DOUBLE) :: ldarr(5), ldarr2(5), ldarrExp(5)

  integer(4) :: errorCount

  integer (C_INT) :: i


  if (float_size() /= C_FLOAT .or. dbl_size() /= C_DOUBLE .or. ldbl_size() /= C_LONG_DOUBLE) then
     print *, "Incorrect float/double/long double size:"
     print *, " float size is ", float_size(), " should be ", C_FLOAT
     print *, " double size is ", dbl_size(), " should be ", C_DOUBLE
     print *, " long double size is ", ldbl_size(), " should be ", C_LONG_DOUBLE
     error stop 20_4
  end if


  errorCount = 0


  farr    = (/ (float_fun(i),i=0,4) /)
  farr2   = (/ float_fun(0), float_fun(1), float_fun(2), float_fun(3), float_fun(4) /)
  farrExp = (/ 1e-5_C_FLOAT, -1e-37_C_FLOAT, +1e37_C_FLOAT, &
                ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF) /)

  do i=1,5
     if (diff4(farr(i),farr2(i),farrExp(i), 'no spec', i)) errorCount = errorCount + 1
  end do

  farr    = (/ real(C_FLOAT):: (float_fun(i),i=0,4) /)
  farr2   = (/ real(C_FLOAT):: float_fun(0), float_fun(1), float_fun(2), float_fun(3), float_fun(4) /)
  farrExp = (/ real(C_FLOAT):: 1e-5_C_FLOAT, -1e-37_C_FLOAT, +1e37_C_FLOAT, &
                ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF) /)

  do i=1,5
     if (diff4(farr(i),farr2(i),farrExp(i), 'type spec', i)) errorCount = errorCount + 1
  end do


  darr    = (/ (double_fun(i),i=0,4) /)
  darr2   = (/ double_fun(0), double_fun(1), double_fun(2), double_fun(3), double_fun(4) /)
  darrExp = (/ 1e-9_C_DOUBLE, -1e-37_C_DOUBLE, +1e37_C_DOUBLE, &
                ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF) /)

  do i=1,5
     if (diff8(darr(i),darr2(i),darrExp(i),'no spec',i)) errorCount = errorCount + 1
  end do

  darr    = (/ real(C_DOUBLE):: (double_fun(i),i=0,4) /)
  darr2   = (/ real(C_DOUBLE):: double_fun(0), double_fun(1), double_fun(2), double_fun(3), double_fun(4) /)
  darrExp = (/ real(C_DOUBLE):: 1e-9_C_DOUBLE, -1e-37_C_DOUBLE, +1e37_C_DOUBLE, &
                ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF) /)

  do i=1,5
     if (diff8(darr(i),darr2(i),darrExp(i),'type spec',i)) errorCount = errorCount + 1
  end do


  ldarr    = (/ (long_double_fun(i),i=0,4) /)
  ldarr2   = (/ long_double_fun(0_C_INT), long_double_fun(1_C_INT), long_double_fun(2_C_INT), long_double_fun(3_C_INT), long_double_fun(4_C_INT) /)
  ldarrExp = (/ 1e-9_C_LONG_DOUBLE, -1e-37_C_LONG_DOUBLE, +1e37_C_LONG_DOUBLE, &
                ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF) /)

  do i=1,5
     if (diff16(ldarr(i),ldarr2(i),ldarrExp(i),'no spec',i)) errorCount = errorCount + 1
  end do

  ldarr    = (/ real(C_LONG_DOUBLE):: (long_double_fun(i),i=0,4) /)
  ldarr2   = (/ real(C_LONG_DOUBLE):: long_double_fun(0_C_INT), long_double_fun(1_C_INT), long_double_fun(2_C_INT), long_double_fun(3_C_INT), long_double_fun(4_C_INT) /)
  ldarrExp = (/ real(C_LONG_DOUBLE):: 1e-9_C_LONG_DOUBLE, -1e-37_C_LONG_DOUBLE, +1e37_C_LONG_DOUBLE,  &
                ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN), ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF) /)

  do i=1,5
     if (diff16(ldarr(i),ldarr2(i),ldarrExp(i),'type spec',i)) errorCount = errorCount + 1
  end do


  if (errorCount > 0) call zzrc(errorCount)

contains

  logical function diff16(v1, v2, exp, txt, id)
    real(16), intent(in) :: v1, v2, exp
    character(*) :: txt
    integer :: id
    logical(4) :: precision_r16, all_nan, same
    ! check first for NaN:
    ! all_nan = (v1 .ne. v1 .and. v2 .ne. v2 .and. exp .ne. exp)
    all_nan = (ieee_is_nan(v1) .and. ieee_is_nan(v2) .and. ieee_is_nan(exp))
    same = all_nan .or. ((v1 == exp .or. precision_r16(v1,exp)) .and. (v2 == exp .or. precision_r16(v2,exp)))
    diff16 = .not. same
    if (diff16) then
        print *, "Long double mismatch / ", txt, id
        print *, "  ",v1
        print *, "  ",v2
        PRINT *, "should be"
        print *, "  ",exp
    end if
    return
  end function diff16

  logical function diff8(v1, v2, exp, txt, id)
    real(8), intent(in) :: v1, v2, exp
    character(*) :: txt
    integer :: id
    logical(4) :: precision_r8, all_nan, same
    ! check first for NaN:
    ! all_nan = (v1 .ne. v1 .and. v2 .ne. v2 .and. exp .ne. exp)
    all_nan = (ieee_is_nan(v1) .and. ieee_is_nan(v2) .and. ieee_is_nan(exp))
    same = all_nan .or. ((v1 == exp .or. precision_r8(v1,exp)) .and. (v2 == exp .or. precision_r8(v2,exp)))
    diff8 = .not. same
    if (diff8) then
        print *, "Double mismatch / ", txt, id
        print *, "  ",v1
        print *, "  ",v2
        PRINT *, "should be"
        print *, "  ",exp
    end if
    return
  end function diff8

  logical function diff4(v1, v2, exp, txt, id)
    real(4), intent(in) :: v1, v2, exp
    character(*) :: txt
    integer :: id
    logical(4) :: precision_r4, all_nan, same
    ! check first for NaN:
    ! all_nan = (v1 .ne. v1 .and. v2 .ne. v2 .and. exp .ne. exp)
    all_nan = (ieee_is_nan(v1) .and. ieee_is_nan(v2) .and. ieee_is_nan(exp))
    same = all_nan .or. ((v1 == exp .or. precision_r4(v1,exp)) .and. (v2 == exp .or. precision_r4(v2,exp)))
    diff4 = .not. same
    if (diff4) then
        print *, "Float mismatch / ", txt, id
        print *, "  ",v1
        print *, "  ",v2
        PRINT *, "should be"
        print *, "  ",exp
    end if
    return
  end function diff4


end program acemc02
