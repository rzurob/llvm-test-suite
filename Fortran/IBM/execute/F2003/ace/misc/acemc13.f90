!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc13
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-21
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values to C functions (complex)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
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

program acemc13

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


     subroutine record_complex_floats(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       complex(C_FLOAT_COMPLEX) :: arr(*)
     end subroutine record_complex_floats

     complex(C_FLOAT_COMPLEX) function get_complex_float(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_complex_float

     subroutine record_complex_doubles(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       complex(C_DOUBLE_COMPLEX) :: arr(*)
     end subroutine record_complex_doubles

     complex(C_DOUBLE_COMPLEX) function get_complex_double(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_complex_double

     subroutine record_complex_long_doubles(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       complex(C_LONG_DOUBLE_COMPLEX) :: arr(*)
     end subroutine record_complex_long_doubles

     complex(C_LONG_DOUBLE_COMPLEX) function get_complex_long_double(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_complex_long_double

  end interface

  complex (C_FLOAT_COMPLEX) ::       farr(7)
  complex (C_DOUBLE_COMPLEX) ::      darr(7)
  complex (C_LONG_DOUBLE_COMPLEX) :: ldarr(7)

  integer(4) :: errorsFound

  integer(C_INT)         :: i
  
  errorsFound = 0

  if (cfloat_size() /= C_FLOAT_COMPLEX .or. cdbl_size() /= C_DOUBLE_COMPLEX .or. cldbl_size() /= C_LONG_DOUBLE_COMPLEX) then
     print *, "Incorrect float/double/long double complex size:"
     print *, " float complex size is ", cfloat_size(), " should be ", C_FLOAT_COMPLEX
     print *, "double complex size is ", cdbl_size(), " should be ", C_DOUBLE_COMPLEX
     print *, " long double complex size is ", cldbl_size(), " should be ", C_LONG_DOUBLE_COMPLEX
     error stop 2_4
  end if


  farr = [(0.0_C_FLOAT_COMPLEX,0.0_C_FLOAT_COMPLEX), &
       & (1.0_C_FLOAT_COMPLEX,1.0_C_FLOAT_COMPLEX), (-0.0_C_FLOAT_COMPLEX,-0.0_C_FLOAT_COMPLEX), &
       & (tiny(0.0_C_FLOAT),tiny(0.0_C_FLOAT)), (huge(0.0_C_FLOAT),huge(0.0_C_FLOAT)), & 
       & (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF))]

  darr = [(0.0_C_DOUBLE_COMPLEX,0.0_C_DOUBLE_COMPLEX), &
       & (1.0_C_DOUBLE_COMPLEX,1.0_C_DOUBLE_COMPLEX), (-0.0_C_DOUBLE_COMPLEX,-0.0_C_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_DOUBLE),tiny(0.0_C_DOUBLE)), (huge(0.0_C_DOUBLE),huge(0.0_C_DOUBLE)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF))]

  ldarr = [(0.0_C_LONG_DOUBLE_COMPLEX,0.0_C_LONG_DOUBLE_COMPLEX), &
       & (1.0_C_LONG_DOUBLE_COMPLEX,1.0_C_LONG_DOUBLE_COMPLEX), (-0.0_C_LONG_DOUBLE_COMPLEX,-0.0_C_LONG_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_LONG_DOUBLE),tiny(0.0_C_LONG_DOUBLE)), (huge(0.0_C_LONG_DOUBLE),huge(0.0_C_LONG_DOUBLE)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF))]


  call record_complex_floats(size(farr), farr)
  call verify_complex_floats
  call record_complex_floats(size(farr), &
       &[(0.0_C_FLOAT_COMPLEX,0.0_C_FLOAT_COMPLEX), &
       & (1.0_C_FLOAT_COMPLEX,1.0_C_FLOAT_COMPLEX), (-0.0_C_FLOAT_COMPLEX,-0.0_C_FLOAT_COMPLEX), &
       & (tiny(0.0_C_FLOAT),tiny(0.0_C_FLOAT)), (huge(0.0_C_FLOAT),huge(0.0_C_FLOAT)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF))])
  call verify_complex_floats
  call record_complex_floats(size(farr), &
       &[((0.0_C_FLOAT_COMPLEX,0.0_C_FLOAT_COMPLEX), &
       & (1.0_C_FLOAT_COMPLEX,1.0_C_FLOAT_COMPLEX), (-0.0_C_FLOAT_COMPLEX,-0.0_C_FLOAT_COMPLEX), &
       & (tiny(0.0_C_FLOAT),tiny(0.0_C_FLOAT)), (huge(0.0_C_FLOAT),huge(0.0_C_FLOAT)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)), i=1,1)])
  call verify_complex_floats
  call record_complex_floats(size(farr), &
       &[complex(C_FLOAT_COMPLEX):: (0.0_C_FLOAT_COMPLEX,0.0_C_FLOAT_COMPLEX), &
       & (1.0_C_FLOAT_COMPLEX,1.0_C_FLOAT_COMPLEX), (-0.0_C_FLOAT_COMPLEX,-0.0_C_FLOAT_COMPLEX), &
       & (tiny(0.0_C_FLOAT),tiny(0.0_C_FLOAT)), (huge(0.0_C_FLOAT),huge(0.0_C_FLOAT)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF))])
  call verify_complex_floats
  call record_complex_floats(size(farr), &
       &[complex(C_FLOAT_COMPLEX):: ((0.0_C_FLOAT_COMPLEX,0.0_C_FLOAT_COMPLEX), &
       & (1.0_C_FLOAT_COMPLEX,1.0_C_FLOAT_COMPLEX), (-0.0_C_FLOAT_COMPLEX,-0.0_C_FLOAT_COMPLEX), &
       & (tiny(0.0_C_FLOAT),tiny(0.0_C_FLOAT)), (huge(0.0_C_FLOAT),huge(0.0_C_FLOAT)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF),ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF)), i=1,1)])
  call verify_complex_floats


  call record_complex_doubles(size(darr), darr)
  call verify_complex_doubles
  call record_complex_doubles(size(darr), &
       &[(0.0_C_DOUBLE_COMPLEX,0.0_C_DOUBLE_COMPLEX), &
       & (1.0_C_DOUBLE_COMPLEX,1.0_C_DOUBLE_COMPLEX), (-0.0_C_DOUBLE_COMPLEX,-0.0_C_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_DOUBLE),tiny(0.0_C_DOUBLE)), (huge(0.0_C_DOUBLE),huge(0.0_C_DOUBLE)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF))])
  call verify_complex_doubles
  call record_complex_doubles(size(darr), &
       &[((0.0_C_DOUBLE_COMPLEX,0.0_C_DOUBLE_COMPLEX), &
       & (1.0_C_DOUBLE_COMPLEX,1.0_C_DOUBLE_COMPLEX), (-0.0_C_DOUBLE_COMPLEX,-0.0_C_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_DOUBLE),tiny(0.0_C_DOUBLE)), (huge(0.0_C_DOUBLE),huge(0.0_C_DOUBLE)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)), i=1,1)])
  call verify_complex_doubles
  call record_complex_doubles(size(darr), &
       &[complex(C_DOUBLE_COMPLEX):: (0.0_C_DOUBLE_COMPLEX,0.0_C_DOUBLE_COMPLEX), &
       & (1.0_C_DOUBLE_COMPLEX,1.0_C_DOUBLE_COMPLEX), (-0.0_C_DOUBLE_COMPLEX,-0.0_C_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_DOUBLE),tiny(0.0_C_DOUBLE)), (huge(0.0_C_DOUBLE),huge(0.0_C_DOUBLE)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF))])
  call verify_complex_doubles
  call record_complex_doubles(size(darr), &
       &[complex(C_DOUBLE_COMPLEX):: ((0.0_C_DOUBLE_COMPLEX,0.0_C_DOUBLE_COMPLEX), &
       & (1.0_C_DOUBLE_COMPLEX,1.0_C_DOUBLE_COMPLEX), (-0.0_C_DOUBLE_COMPLEX,-0.0_C_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_DOUBLE),tiny(0.0_C_DOUBLE)), (huge(0.0_C_DOUBLE),huge(0.0_C_DOUBLE)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF)), i=1,1)])
  call verify_complex_doubles


  call record_complex_long_doubles(size(ldarr), ldarr)
  call verify_complex_long_doubles
  call record_complex_long_doubles(size(ldarr), &
       &[(0.0_C_LONG_DOUBLE_COMPLEX,0.0_C_LONG_DOUBLE_COMPLEX), &
       & (1.0_C_LONG_DOUBLE_COMPLEX,1.0_C_LONG_DOUBLE_COMPLEX), (-0.0_C_LONG_DOUBLE_COMPLEX,-0.0_C_LONG_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_LONG_DOUBLE),tiny(0.0_C_LONG_DOUBLE)), (huge(0.0_C_LONG_DOUBLE),huge(0.0_C_LONG_DOUBLE)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF))])
  call verify_complex_long_doubles
  call record_complex_long_doubles(size(ldarr), &
       &[((0.0_C_LONG_DOUBLE_COMPLEX,0.0_C_LONG_DOUBLE_COMPLEX), &
       & (1.0_C_LONG_DOUBLE_COMPLEX,1.0_C_LONG_DOUBLE_COMPLEX), (-0.0_C_LONG_DOUBLE_COMPLEX,-0.0_C_LONG_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_LONG_DOUBLE),tiny(0.0_C_LONG_DOUBLE)), (huge(0.0_C_LONG_DOUBLE),huge(0.0_C_LONG_DOUBLE)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)), i=1,1)])
  call verify_complex_long_doubles
  call record_complex_long_doubles(size(ldarr), &
       &[complex(C_LONG_DOUBLE_COMPLEX):: (0.0_C_LONG_DOUBLE_COMPLEX,0.0_C_LONG_DOUBLE_COMPLEX), &
       & (1.0_C_LONG_DOUBLE_COMPLEX,1.0_C_LONG_DOUBLE_COMPLEX), (-0.0_C_LONG_DOUBLE_COMPLEX,-0.0_C_LONG_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_LONG_DOUBLE),tiny(0.0_C_LONG_DOUBLE)), (huge(0.0_C_LONG_DOUBLE),huge(0.0_C_LONG_DOUBLE)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF))])
  call verify_complex_long_doubles
  call record_complex_long_doubles(size(ldarr), &
       &[complex(C_LONG_DOUBLE_COMPLEX):: ((0.0_C_LONG_DOUBLE_COMPLEX,0.0_C_LONG_DOUBLE_COMPLEX), &
       & (1.0_C_LONG_DOUBLE_COMPLEX,1.0_C_LONG_DOUBLE_COMPLEX), (-0.0_C_LONG_DOUBLE_COMPLEX,-0.0_C_LONG_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_LONG_DOUBLE),tiny(0.0_C_LONG_DOUBLE)), (huge(0.0_C_LONG_DOUBLE),huge(0.0_C_LONG_DOUBLE)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF)), i=1,1)])
  call verify_complex_long_doubles


  if (errorsFound /= 0) call zzrc (10_4 + errorsFound)

  !! Now perform a self-test: make sure that we can detect the apparently incorrect recording of values:
  print *, "Self-test: expect 10 mismatches (4 float, 3 double, 3 long double):"

  call record_complex_floats(size(farr), &
       &[(1.0_C_FLOAT_COMPLEX,0.0_C_FLOAT_COMPLEX), &
       & (1.0_C_FLOAT_COMPLEX,2.0_C_FLOAT_COMPLEX), (-0.0_C_FLOAT_COMPLEX,-0.0_C_FLOAT_COMPLEX), &
       & (tiny(0.0_C_FLOAT),tiny(0.0_C_FLOAT)), (1e37_C_FLOAT,huge(0.0_C_FLOAT)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_FLOAT, IEEE_POSITIVE_INF),1e37_C_FLOAT)])
  call verify_complex_floats
  call record_complex_doubles(size(darr), &
       &[(0.0_C_DOUBLE_COMPLEX,0.0_C_DOUBLE_COMPLEX), &
       & (3.0_C_DOUBLE_COMPLEX,1.0_C_DOUBLE_COMPLEX), (-0.0_C_DOUBLE_COMPLEX,-4.0_C_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_DOUBLE),tiny(0.0_C_DOUBLE)), (huge(0.0_C_DOUBLE),huge(0.0_C_DOUBLE)), &
       & (0.0_C_DOUBLE,ieee_value(0.0_C_DOUBLE, IEEE_QUIET_NAN)), &
       & (ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_DOUBLE, IEEE_POSITIVE_INF))])
  call verify_complex_doubles
  call record_complex_long_doubles(size(ldarr), &
       &[(0.0_C_LONG_DOUBLE_COMPLEX,0.0_C_LONG_DOUBLE_COMPLEX), &
       & (1.0_C_LONG_DOUBLE_COMPLEX,1.0_C_LONG_DOUBLE_COMPLEX), (-5.0_C_LONG_DOUBLE_COMPLEX,-6.0_C_LONG_DOUBLE_COMPLEX), &
       & (tiny(0.0_C_LONG_DOUBLE),0.0_C_LONG_DOUBLE), (huge(0.0_C_LONG_DOUBLE),huge(0.0_C_LONG_DOUBLE)), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_QUIET_NAN),0.0_C_LONG_DOUBLE), &
       & (ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF),ieee_value(0.0_C_LONG_DOUBLE, IEEE_POSITIVE_INF))])
  call verify_complex_long_doubles

  if (errorsFound /= 10) error stop 4_4


contains

  subroutine verify_complex_floats
    integer :: i
    logical :: realError, imagError
    do i=1,size(farr)
       if (farr(i) /= get_complex_float(i)) then
          realError = (real(farr(i)) /= real(get_complex_float(i)) .and. .not. (ieee_is_nan(real(farr(i))) .and. ieee_is_nan(real(get_complex_float(i)))))
          imagError = (aimag(farr(i)) /= aimag(get_complex_float(i)) .and. .not. (ieee_is_nan(aimag(farr(i))) .and. ieee_is_nan(aimag(get_complex_float(i)))))
          if (realError .or. imagError) then
             errorsFound = errorsFound + 1
             print *, "Float complex mismatch:", farr(i), "/=", get_complex_float(i)
          end if
       end if
    end do
  end subroutine verify_complex_floats

  subroutine verify_complex_doubles
    integer :: i
    logical :: realError, imagError
    do i=1,size(darr)
       if (darr(i) /= get_complex_double(i)) then
          realError = (real(darr(i)) /= real(get_complex_double(i)) .and. .not. (ieee_is_nan(real(darr(i))) .and. ieee_is_nan(real(get_complex_double(i)))))
          imagError = (aimag(darr(i)) /= aimag(get_complex_double(i)) .and. .not. (ieee_is_nan(aimag(darr(i))) .and. ieee_is_nan(aimag(get_complex_double(i)))))
          if (realError .or. imagError) then
             errorsFound = errorsFound + 1
             print *, "Double complex mismatch:", darr(i), "/=", get_complex_double(i)
          end if
       end if
    end do
  end subroutine verify_complex_doubles

  subroutine verify_complex_long_doubles
    integer :: i
    logical :: realError, imagError
    do i=1,size(ldarr)
       if (ldarr(i) /= get_complex_long_double(i)) then
          realError = (real(ldarr(i)) /= real(get_complex_long_double(i)) .and. .not. (ieee_is_nan(real(ldarr(i))) .and. ieee_is_nan(real(get_complex_long_double(i)))))
          imagError = (aimag(ldarr(i)) /= aimag(get_complex_long_double(i)) .and. .not. (ieee_is_nan(aimag(ldarr(i))) .and. ieee_is_nan(aimag(get_complex_long_double(i)))))
          if (realError .or. imagError) then
             errorsFound = errorsFound + 1
             print *, "Long double complex mismatch:", ldarr(i), "/=", get_complex_long_double(i)
          end if
       end if
    end do
  end subroutine verify_complex_long_doubles

end program acemc13
