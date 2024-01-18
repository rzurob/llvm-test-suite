!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc12
!*
!*  DATE                       : 2006-07-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values to C functions (real)
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
!*  Assign AC's to FORTRAN array variables, then pass the same AC's to a routine
!*  in C which stores them, and test the values stored in C against the FORTRAN
!*  arrays.  The test is successful if the values are correct.
!*
!*  The selftest works by intentionally passing the wrong values to C, and
!*  verifying that the error is detected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc12

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


     subroutine record_floats(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       real(C_FLOAT) :: arr(*)
     end subroutine record_floats

     real(C_FLOAT) function get_float(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_float

     subroutine record_doubles(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       real(C_DOUBLE) :: arr(*)
     end subroutine record_doubles

     real(C_DOUBLE) function get_double(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_double

     subroutine record_long_doubles(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       real(C_LONG_DOUBLE) :: arr(*)
     end subroutine record_long_doubles

     real(C_LONG_DOUBLE) function get_long_double(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_long_double

  end interface

  real (C_FLOAT) ::       farr(7)
  real (C_DOUBLE) ::      darr(7)
  real (C_LONG_DOUBLE) :: ldarr(7)

  integer(4) :: errorsFound

  integer(C_INT)         :: i

  errorsFound = 0

  if (float_size() /= C_FLOAT .or. dbl_size() /= C_DOUBLE .or. ldbl_size() /= C_LONG_DOUBLE) then
     print *, "Incorrect float/double/long double size:"
     print *, " float size is ", float_size(), " should be ", C_FLOAT
     print *, " double size is ", dbl_size(), " should be ", C_DOUBLE
     print *, " long double size is ", ldbl_size(), " should be ", C_LONG_DOUBLE
     error stop 2_4
  end if

  farr = [0.0_C_FLOAT, 1.0_C_FLOAT, -0.0_C_FLOAT, &
       & tiny(0.0_C_FLOAT), huge(0.0_C_FLOAT), &
       & ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)]
  darr = [0.0_C_DOUBLE, 1.0_C_DOUBLE, -0.0_C_DOUBLE, &
       & tiny(0.0_C_DOUBLE), huge(0.0_C_DOUBLE), &
       & ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)]
  ldarr = [0.0_C_LONG_DOUBLE, 1.0_C_LONG_DOUBLE, -0.0_C_LONG_DOUBLE, &
       & tiny(0.0_C_LONG_DOUBLE), huge(0.0_C_LONG_DOUBLE), &
       & ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)]

  call record_floats(size(farr), farr)
  call verify_floats
  call record_floats(size(farr), &
       &[0.0_C_FLOAT, 1.0_C_FLOAT, -0.0_C_FLOAT, &
       & tiny(0.0_C_FLOAT), huge(0.0_C_FLOAT), &
       & ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)])
  call verify_floats
  call record_floats(size(farr), &
       &[(0.0_C_FLOAT, 1.0_C_FLOAT, -0.0_C_FLOAT, &
       & tiny(0.0_C_FLOAT), huge(0.0_C_FLOAT), &
       & ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF), i=1,1)])
  call verify_floats
  call record_floats(size(farr), &
       &[real(C_FLOAT):: 0.0_C_FLOAT, 1.0_C_FLOAT, -0.0_C_FLOAT, &
       & tiny(0.0_C_FLOAT), huge(0.0_C_FLOAT), &
       & ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)])
  call verify_floats
  call record_floats(size(farr), &
       &[real(C_FLOAT):: (0.0_C_FLOAT, 1.0_C_FLOAT, -0.0_C_FLOAT, &
       & tiny(0.0_C_FLOAT), huge(0.0_C_FLOAT), &
       & ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF), i=1,1)])
  call verify_floats

  call record_doubles(size(darr), darr)
  call verify_doubles
  call record_doubles(size(darr), &
       &[0.0_C_DOUBLE, 1.0_C_DOUBLE, -0.0_C_DOUBLE, &
       & tiny(0.0_C_DOUBLE), huge(0.0_C_DOUBLE), &
       & ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)])
  call verify_doubles
  call record_doubles(size(darr), &
       &[(0.0_C_DOUBLE, 1.0_C_DOUBLE, -0.0_C_DOUBLE, &
       & tiny(0.0_C_DOUBLE), huge(0.0_C_DOUBLE), &
       & ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF), i=1,1)])
  call verify_doubles
  call record_doubles(size(darr), &
       &[real(C_DOUBLE):: 0.0_C_DOUBLE, 1.0_C_DOUBLE, -0.0_C_DOUBLE, &
       & tiny(0.0_C_DOUBLE), huge(0.0_C_DOUBLE), &
       & ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)])
  call verify_doubles
  call record_doubles(size(darr), &
       &[real(C_DOUBLE):: (0.0_C_DOUBLE, 1.0_C_DOUBLE, -0.0_C_DOUBLE, &
       & tiny(0.0_C_DOUBLE), huge(0.0_C_DOUBLE), &
       & ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF), i=1,1)])
  call verify_doubles

  call record_long_doubles(size(ldarr), ldarr)
  call verify_long_doubles
  call record_long_doubles(size(ldarr), &
       &[0.0_C_LONG_DOUBLE, 1.0_C_LONG_DOUBLE, -0.0_C_LONG_DOUBLE, &
       & tiny(0.0_C_LONG_DOUBLE), huge(0.0_C_LONG_DOUBLE), &
       & ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)])
  call verify_long_doubles
  call record_long_doubles(size(ldarr), &
       &[(0.0_C_LONG_DOUBLE, 1.0_C_LONG_DOUBLE, -0.0_C_LONG_DOUBLE, &
       & tiny(0.0_C_LONG_DOUBLE), huge(0.0_C_LONG_DOUBLE), &
       & ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF), i=1,1)])
  call verify_long_doubles
  call record_long_doubles(size(ldarr), &
       &[real(C_LONG_DOUBLE):: 0.0_C_LONG_DOUBLE, 1.0_C_LONG_DOUBLE, -0.0_C_LONG_DOUBLE, &
       & tiny(0.0_C_LONG_DOUBLE), huge(0.0_C_LONG_DOUBLE), &
       & ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)])
  call verify_long_doubles
  call record_long_doubles(size(ldarr), &
       &[real(C_LONG_DOUBLE):: (0.0_C_LONG_DOUBLE, 1.0_C_LONG_DOUBLE, -0.0_C_LONG_DOUBLE, &
       & tiny(0.0_C_LONG_DOUBLE), huge(0.0_C_LONG_DOUBLE), &
       & ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF), i=1,1)])
  call verify_long_doubles


  if (errorsFound /= 0) call zzrc (10_4 + errorsFound)

  !! Now perform a self-test: make sure that we can detect the apparently incorrect recording of values:
  print *, "Self-test: expect 5 mismatches (1 float, 1 double, 3 long double):"

  call record_floats(size(farr), &
       &[real(C_FLOAT):: 1.0_C_FLOAT, 1.0_C_FLOAT, -0.0_C_FLOAT, &
       & tiny(0.0_C_FLOAT), huge(0.0_C_FLOAT), &
       & ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)])
  call verify_floats
  call record_doubles(size(darr), &
       &[real(C_DOUBLE):: 0.0_C_DOUBLE, 2.0_C_DOUBLE, -0.0_C_DOUBLE, &
       & tiny(0.0_C_DOUBLE), huge(0.0_C_DOUBLE), &
       & ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)])
  call verify_doubles
  call record_long_doubles(size(ldarr), &
       &[real(C_LONG_DOUBLE):: 0.0_C_LONG_DOUBLE, 1.0_C_LONG_DOUBLE, -3.0_C_LONG_DOUBLE, &
       & tiny(0.0_C_LONG_DOUBLE), huge(0.0_C_LONG_DOUBLE), &
       & ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF), ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)])
  call verify_long_doubles

  if (errorsFound /= 5) error stop 4_4


contains

  subroutine verify_floats
    integer(4) :: ecode, i
    do i=1,size(farr)
       if (farr(i) /= get_float(i) .and. .not. (ieee_is_nan(farr(i)) .and. ieee_is_nan(get_float(i)))) then
          errorsFound = errorsFound + 1
          print *, "Float mismatch:", farr(i), "/=", get_float(i)
       end if
    end do
  end subroutine verify_floats

  subroutine verify_doubles
    integer(4) :: ecode, i
    do i=1,size(darr)
       if (darr(i) /= get_double(i) .and. .not. (ieee_is_nan(darr(i)) .and. ieee_is_nan(get_double(i)))) then
          errorsFound = errorsFound + 1
          print *, "Double mismatch:", darr(i), "/=", get_double(i)
       end if
    end do
  end subroutine verify_doubles

  subroutine verify_long_doubles
    integer(4) :: ecode, i
    do i=1,size(ldarr)
       if (ldarr(i) /= get_long_double(i) .and. .not. (ieee_is_nan(ldarr(i)) .and. ieee_is_nan(get_long_double(i)))) then
          errorsFound = errorsFound + 1
          print *, "Long double mismatch:", ldarr(i), "/=", get_long_double(i)
       end if
    end do
  end subroutine verify_long_doubles

end program acemc12
