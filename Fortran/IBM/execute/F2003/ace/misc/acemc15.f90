!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc15
!*
!*  DATE                       : 2006-10-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: derived type values to C functions
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
!*  In AC, invoke C functions returning various types of values, and verify
!*  their correctness.
!*
!*  The test is successful if the values are correct.
!*
!*  Note that the C and FORTRAN files have to be correctly compiled so that
!*  numbers have the correct size, etc. (in XLC, use options -qlonglong and -qldbl128;
!*  if using GCC, make sure the FORTRAN compiler uses option -qfloat=complexgcc).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc15
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  type, bind(c) :: complex_stuff
    complex(C_LONG_DOUBLE) :: long_double_complex_field
    complex(C_DOUBLE)      :: double_complex_field
    complex(C_FLOAT)       :: float_complex_field
  end type complex_stuff

  type, bind(c) :: fp_stuff
    real(C_LONG_DOUBLE) :: long_double_field
    real(C_DOUBLE)      :: double_field
    real(C_FLOAT)       :: float_field
  end type fp_stuff

  type, bind(c) :: int_stuff
    integer(C_LONG_LONG)   :: long_long_field
    integer(C_LONG)        :: long_field
    integer(C_INT)         :: int_field
    integer(C_SHORT)       :: short_field
    integer(C_SIGNED_CHAR) :: signed_char_field
  end type int_stuff

  type, bind(c) :: foreign
    type (complex_stuff) :: complex_stuff_field
    type (fp_stuff)      :: fp_stuff_field
    type (int_stuff)     :: int_stuff_field
    logical(C_BOOL)      :: bool_field
    character(C_CHAR)    :: char_field
  end type foreign

  interface

     type (complex_stuff) function get_complex_stuff(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import complex_stuff
       integer (C_INT), value :: inx
     end function get_complex_stuff

     subroutine record_complex_stuff(inx, items) bind (c)
       use, intrinsic :: iso_c_binding
       import complex_stuff
       integer (C_INT), value :: inx
       type (complex_stuff) :: items(*)
     end subroutine record_complex_stuff


     type (fp_stuff) function get_fp_stuff(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import fp_stuff
       integer (C_INT), value :: inx
     end function get_fp_stuff

     subroutine record_fp_stuff(inx, items) bind (c)
       use, intrinsic :: iso_c_binding
       import fp_stuff
       integer (C_INT), value :: inx
       type (fp_stuff) :: items(*)
     end subroutine record_fp_stuff


     type (int_stuff) function get_int_stuff(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import int_stuff
       integer(C_INT), value       :: inx
     end function get_int_stuff

     subroutine record_int_stuff(inx, items) bind (c)
       use, intrinsic :: iso_c_binding
       import int_stuff
       integer(C_INT), value       :: inx
       type (int_stuff) :: items(*)
     end subroutine record_int_stuff


     type (foreign) function get_foreign(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import foreign
       integer(C_INT), value :: inx
     end function get_foreign

     subroutine record_foreign(inx, items) bind (c)
       use, intrinsic :: iso_c_binding
       import foreign
       integer(C_INT), value :: inx
       type (foreign) :: items(*)
     end subroutine record_foreign

  end interface

  type (int_stuff) :: int_stuff_array(5)
  type (fp_stuff) :: fp_stuff_array(5)
  type (complex_stuff) :: complex_stuff_array(5)
  type (foreign) :: foreign_array(5)
  integer :: i
  integer(4) :: errorsFound
  integer :: bool_size, cdbl_size, cfloat_size, char_size, cldbl_size, dbl_size, &
       float_size, int_size, ldbl_size, llong_size, long_size, schar_size, short_size
  external :: bool_size, cdbl_size, cfloat_size, char_size, cldbl_size, dbl_size, &
       float_size, int_size, ldbl_size, llong_size, long_size, schar_size, short_size

  errorsFound = 0

  ! certain routines invoke precision_r4 on real(C_FLOAT) data, etc.,
  ! so they won't work if the size is incorrect, meaning the test is then invalid:
  if (C_FLOAT /= 4 .or. C_DOUBLE /= 8 .or. C_LONG_DOUBLE /= 16) then
     print *, 'Error in expected real sizes:'
     print *, '  C_FLOAT should be 4, is',C_FLOAT
     print *, '  C_DOUBLE should be 8, is', C_DOUBLE
     print *, '  C_LONG_DOUBLE should be 16, is', C_LONG_DOUBLE
     stop 2
  end if

  if (bool_size() /= C_BOOL .OR. char_size() /= C_CHAR .or. schar_size() /= C_SIGNED_CHAR &
       .or. int_size() /= C_INT .or. short_size() /= C_SHORT .or. long_size() /= C_LONG .or. llong_size() /= C_LONG_LONG &
       .or. float_size() /= C_FLOAT .or. dbl_size() /= C_DOUBLE .or. ldbl_size() /= C_LONG_DOUBLE &
       .or. cfloat_size() /= C_FLOAT_COMPLEX .or. cdbl_size() /= C_DOUBLE_COMPLEX .or. cldbl_size() /= C_LONG_DOUBLE_COMPLEX) then
     print *, "Unexpected size:"
     print *, "  C_BOOL is", C_BOOL, "; C delivers", bool_size()
     print *, "  C_CHAR is", C_CHAR, "; C delivers", char_size()
     print *, "  C_SIGNED_CHAR is", C_SIGNED_CHAR, "; C delivers", schar_size()
     print *, "  C_INT is", C_INT, "; C delivers", int_size()
     print *, "  C_SHORT is", C_SHORT, "; C delivers", short_size()
     print *, "  C_LONG is", C_LONG, "; C delivers", long_size()
     print *, "  C_LONG_LONG is", C_LONG_LONG, "; C delivers", llong_size()
     print *, "  C_FLOAT is", C_FLOAT, "; C delivers", float_size()
     print *, "  C_DOUBLE is", C_DOUBLE, "; C delivers", dbl_size()
     print *, "  C_LONG_DOUBLE is", C_LONG_DOUBLE, "; C delivers", ldbl_size()
     print *, "  C_FLOAT_COMPLEX is", C_FLOAT_COMPLEX, "; C delivers", cfloat_size()
     print *, "  C_DOUBLE_COMPLEX is", C_DOUBLE_COMPLEX, "; C delivers", cdbl_size()
     print *, "  C_LONG_DOUBLE_COMPLEX is", C_LONG_DOUBLE_COMPLEX, "; C delivers", cldbl_size()
     stop 3
  end if

  int_stuff_array = (/ int_stuff(9223372036854775807_C_LONG_LONG, 2147483647_C_LONG, 32767_C_INT, 32767_C_SHORT, 127_C_SIGNED_CHAR), &
                     & int_stuff(-9223372036854775807_C_LONG_LONG, -2147483647_C_LONG, -32767_C_INT, -32767_C_SHORT, -127_C_SIGNED_CHAR), &
                     & int_stuff(-1_C_LONG_LONG, -1_C_LONG, -1_C_INT, -1_C_SHORT, -1_C_SIGNED_CHAR), &
                     & int_stuff( 0_C_LONG_LONG,  0_C_LONG,  0_C_INT,  0_C_SHORT,  0_C_SIGNED_CHAR), &
                     & int_stuff( 1_C_LONG_LONG,  1_C_LONG,  1_C_INT,  1_C_SHORT,  1_C_SIGNED_CHAR)/)

  call record_int_stuff(size(int_stuff_array), int_stuff_array)
  call verify_int_stuff

  call record_int_stuff(size(int_stuff_array), &
                        (/ int_stuff(9223372036854775807_C_LONG_LONG, 2147483647_C_LONG, 32767_C_INT, 32767_C_SHORT, 127_C_SIGNED_CHAR), &
                         & int_stuff(-9223372036854775807_C_LONG_LONG, -2147483647_C_LONG, -32767_C_INT, -32767_C_SHORT, -127_C_SIGNED_CHAR), &
                         & int_stuff(-1_C_LONG_LONG, -1_C_LONG, -1_C_INT, -1_C_SHORT, -1_C_SIGNED_CHAR), &
                         & int_stuff( 0_C_LONG_LONG,  0_C_LONG,  0_C_INT,  0_C_SHORT,  0_C_SIGNED_CHAR), &
                         & int_stuff( 1_C_LONG_LONG,  1_C_LONG,  1_C_INT,  1_C_SHORT,  1_C_SIGNED_CHAR)/))
  call verify_int_stuff

  call record_int_stuff(size(int_stuff_array), &
                        (/ (int_stuff(9223372036854775807_C_LONG_LONG, 2147483647_C_LONG, 32767_C_INT, 32767_C_SHORT, 127_C_SIGNED_CHAR), &
                         &  int_stuff(-9223372036854775807_C_LONG_LONG, -2147483647_C_LONG, -32767_C_INT, -32767_C_SHORT, -127_C_SIGNED_CHAR), &
                         &  int_stuff(-1_C_LONG_LONG, -1_C_LONG, -1_C_INT, -1_C_SHORT, -1_C_SIGNED_CHAR), &
                         &  int_stuff( 0_C_LONG_LONG,  0_C_LONG,  0_C_INT,  0_C_SHORT,  0_C_SIGNED_CHAR), &
                         &  int_stuff( 1_C_LONG_LONG,  1_C_LONG,  1_C_INT,  1_C_SHORT,  1_C_SIGNED_CHAR), i=1,1)/))
  call verify_int_stuff

  call record_int_stuff(size(int_stuff_array), &
                        (/ int_stuff:: &
                         & int_stuff(9223372036854775807_C_LONG_LONG, 2147483647_C_LONG, 32767_C_INT, 32767_C_SHORT, 127_C_SIGNED_CHAR), &
                         & int_stuff(-9223372036854775807_C_LONG_LONG, -2147483647_C_LONG, -32767_C_INT, -32767_C_SHORT, -127_C_SIGNED_CHAR), &
                         & int_stuff(-1_C_LONG_LONG, -1_C_LONG, -1_C_INT, -1_C_SHORT, -1_C_SIGNED_CHAR), &
                         & int_stuff( 0_C_LONG_LONG,  0_C_LONG,  0_C_INT,  0_C_SHORT,  0_C_SIGNED_CHAR), &
                         & int_stuff( 1_C_LONG_LONG,  1_C_LONG,  1_C_INT,  1_C_SHORT,  1_C_SIGNED_CHAR)/))
  call verify_int_stuff

  call record_int_stuff(size(int_stuff_array), &
                        (/ int_stuff:: &
                         & (int_stuff(9223372036854775807_C_LONG_LONG, 2147483647_C_LONG, 32767_C_INT, 32767_C_SHORT, 127_C_SIGNED_CHAR), &
                         &  int_stuff(-9223372036854775807_C_LONG_LONG, -2147483647_C_LONG, -32767_C_INT, -32767_C_SHORT, -127_C_SIGNED_CHAR), &
                         &  int_stuff(-1_C_LONG_LONG, -1_C_LONG, -1_C_INT, -1_C_SHORT, -1_C_SIGNED_CHAR), &
                         &  int_stuff( 0_C_LONG_LONG,  0_C_LONG,  0_C_INT,  0_C_SHORT,  0_C_SIGNED_CHAR), &
                         &  int_stuff( 1_C_LONG_LONG,  1_C_LONG,  1_C_INT,  1_C_SHORT,  1_C_SIGNED_CHAR), i=1,1)/))
  call verify_int_stuff



  fp_stuff_array = (/ fp_stuff(-9e-100_C_LONG_DOUBLE,-8.0e30_C_DOUBLE,-7.0e20_C_FLOAT), &
		    & fp_stuff(-0.0_C_LONG_DOUBLE, -0.0_C_DOUBLE, -0.0_C_FLOAT), &
		    & fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF)), &
		    & fp_stuff(9.9e200_C_LONG_DOUBLE, 2.1e-30_C_DOUBLE, 0.3e-9_C_FLOAT), &
		    & fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN))/)

  call record_fp_stuff(size(fp_stuff_array), fp_stuff_array)
  call verify_fp_stuff

  call record_fp_stuff(size(fp_stuff_array), &
                       (/ fp_stuff(-9e-100_C_LONG_DOUBLE,-8.0e30_C_DOUBLE,-7.0e20_C_FLOAT), &
                        & fp_stuff(-0.0_C_LONG_DOUBLE, -0.0_C_DOUBLE, -0.0_C_FLOAT), &
                        & fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF)), &
                        & fp_stuff(9.9e200_C_LONG_DOUBLE, 2.1e-30_C_DOUBLE, 0.3e-9_C_FLOAT), &
                        & fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN))/))
  call verify_fp_stuff

  call record_fp_stuff(size(fp_stuff_array), &
                       (/ (fp_stuff(-9e-100_C_LONG_DOUBLE,-8.0e30_C_DOUBLE,-7.0e20_C_FLOAT), &
                        &  fp_stuff(-0.0_C_LONG_DOUBLE, -0.0_C_DOUBLE, -0.0_C_FLOAT), &
                        &  fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF)), &
                        &  fp_stuff(9.9e200_C_LONG_DOUBLE, 2.1e-30_C_DOUBLE, 0.3e-9_C_FLOAT), &
                        &  fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN)), i=1,1)/))
  call verify_fp_stuff

  call record_fp_stuff(size(fp_stuff_array), &
                       (/ fp_stuff:: &
                        & fp_stuff(-9e-100_C_LONG_DOUBLE,-8.0e30_C_DOUBLE,-7.0e20_C_FLOAT), &
                        & fp_stuff(-0.0_C_LONG_DOUBLE, -0.0_C_DOUBLE, -0.0_C_FLOAT), &
                        & fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF)), &
                        & fp_stuff(9.9e200_C_LONG_DOUBLE, 2.1e-30_C_DOUBLE, 0.3e-9_C_FLOAT), &
                        & fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN))/))
  call verify_fp_stuff

  call record_fp_stuff(size(fp_stuff_array), &
                       (/ fp_stuff:: &
                        & (fp_stuff(-9e-100_C_LONG_DOUBLE,-8.0e30_C_DOUBLE,-7.0e20_C_FLOAT), &
                        &  fp_stuff(-0.0_C_LONG_DOUBLE, -0.0_C_DOUBLE, -0.0_C_FLOAT), &
                        &  fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF)), &
                        &  fp_stuff(9.9e200_C_LONG_DOUBLE, 2.1e-30_C_DOUBLE, 0.3e-9_C_FLOAT), &
                        &  fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN)), i=1,1)/))
  call verify_fp_stuff

  complex_stuff_array = (/ complex_stuff((1e1_C_LONG_DOUBLE,2e2_C_LONG_DOUBLE), (3.0_C_DOUBLE,4.0_C_DOUBLE), (0.5_C_FLOAT,0.6_C_FLOAT)), &
                         & complex_stuff((-1.0_C_LONG_DOUBLE,2.0_C_LONG_DOUBLE), (3.0_C_DOUBLE,-4.0_C_DOUBLE), (-5.0_C_FLOAT,-6.0_C_FLOAT)), &
                         & complex_stuff((1e200_C_LONG_DOUBLE,2e-200_C_LONG_DOUBLE), (3.0e37_C_DOUBLE,4.0e-37_C_DOUBLE), (0.5e10_C_FLOAT,0.6e-10_C_FLOAT)), &
                         & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF)), &
                         &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF)), &
                         &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF))), &
                         & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN)), &
                         &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN)), &
                         &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN)))/)

  call record_complex_stuff(size(complex_stuff_array), complex_stuff_array)
  call verify_complex_stuff

  call record_complex_stuff(size(complex_stuff_array), &
                            (/ complex_stuff((1e1_C_LONG_DOUBLE,2e2_C_LONG_DOUBLE), (3.0_C_DOUBLE,4.0_C_DOUBLE), (0.5_C_FLOAT,0.6_C_FLOAT)), &
                             & complex_stuff((-1.0_C_LONG_DOUBLE,2.0_C_LONG_DOUBLE), (3.0_C_DOUBLE,-4.0_C_DOUBLE), (-5.0_C_FLOAT,-6.0_C_FLOAT)), &
                             & complex_stuff((1e200_C_LONG_DOUBLE,2e-200_C_LONG_DOUBLE), (3.0e37_C_DOUBLE,4.0e-37_C_DOUBLE), (0.5e10_C_FLOAT,0.6e-10_C_FLOAT)), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF))), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN)))/))
  call verify_complex_stuff

  call record_complex_stuff(size(complex_stuff_array), &
                            (/(complex_stuff((1e1_C_LONG_DOUBLE,2e2_C_LONG_DOUBLE), (3.0_C_DOUBLE,4.0_C_DOUBLE), (0.5_C_FLOAT,0.6_C_FLOAT)), &
                             & complex_stuff((-1.0_C_LONG_DOUBLE,2.0_C_LONG_DOUBLE), (3.0_C_DOUBLE,-4.0_C_DOUBLE), (-5.0_C_FLOAT,-6.0_C_FLOAT)), &
                             & complex_stuff((1e200_C_LONG_DOUBLE,2e-200_C_LONG_DOUBLE), (3.0e37_C_DOUBLE,4.0e-37_C_DOUBLE), (0.5e10_C_FLOAT,0.6e-10_C_FLOAT)), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF))), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN))), i=1,1)/))
  call verify_complex_stuff

  call record_complex_stuff(size(complex_stuff_array), &
                            (/ complex_stuff:: &
                             & complex_stuff((1e1_C_LONG_DOUBLE,2e2_C_LONG_DOUBLE), (3.0_C_DOUBLE,4.0_C_DOUBLE), (0.5_C_FLOAT,0.6_C_FLOAT)), &
                             & complex_stuff((-1.0_C_LONG_DOUBLE,2.0_C_LONG_DOUBLE), (3.0_C_DOUBLE,-4.0_C_DOUBLE), (-5.0_C_FLOAT,-6.0_C_FLOAT)), &
                             & complex_stuff((1e200_C_LONG_DOUBLE,2e-200_C_LONG_DOUBLE), (3.0e37_C_DOUBLE,4.0e-37_C_DOUBLE), (0.5e10_C_FLOAT,0.6e-10_C_FLOAT)), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF))), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN)))/))
  call verify_complex_stuff

  call record_complex_stuff(size(complex_stuff_array), &
                            (/ complex_stuff:: &
                             &(complex_stuff((1e1_C_LONG_DOUBLE,2e2_C_LONG_DOUBLE), (3.0_C_DOUBLE,4.0_C_DOUBLE), (0.5_C_FLOAT,0.6_C_FLOAT)), &
                             & complex_stuff((-1.0_C_LONG_DOUBLE,2.0_C_LONG_DOUBLE), (3.0_C_DOUBLE,-4.0_C_DOUBLE), (-5.0_C_FLOAT,-6.0_C_FLOAT)), &
                             & complex_stuff((1e200_C_LONG_DOUBLE,2e-200_C_LONG_DOUBLE), (3.0e37_C_DOUBLE,4.0e-37_C_DOUBLE), (0.5e10_C_FLOAT,0.6e-10_C_FLOAT)), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF))), &
                             & complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN)), &
                             &  (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN))), i=1,1)/))
  call verify_complex_stuff


  foreign_array = (/ (foreign(complex_stuff_array(i), fp_stuff_array(i), int_stuff_array(i), mod(i-1,2)==0, achar(iachar('a')+i-1)), i=1,5) /)
  call record_foreign(size(foreign_array), foreign_array)
  call verify_foreign

  call record_foreign(size(foreign_array), &
                            (/ foreign(complex_stuff_array(1), fp_stuff_array(1), int_stuff_array(1),  .true., 'a'), &
                             & foreign(complex_stuff_array(2), fp_stuff_array(2), int_stuff_array(2), .false., 'b'), &
                             & foreign(complex_stuff_array(3), fp_stuff_array(3), int_stuff_array(3),  .true., 'c'), &
                             & foreign(complex_stuff_array(4), fp_stuff_array(4), int_stuff_array(4), .false., 'd'), &
                             & foreign(complex_stuff_array(5), fp_stuff_array(5), int_stuff_array(5),  .true., 'e')/))
  call verify_foreign

  call record_foreign(size(foreign_array), &
                            (/ (foreign(complex_stuff_array(i), fp_stuff_array(i), int_stuff_array(i), mod(i-1,2)==0, achar(iachar('a')+i-1)), i=1,5) /))
  call verify_foreign

  call record_foreign(size(foreign_array), &
                            (/ foreign:: &
                             & foreign(complex_stuff_array(1), fp_stuff_array(1), int_stuff_array(1),  .true., 'a'), &
                             & foreign(complex_stuff_array(2), fp_stuff_array(2), int_stuff_array(2), .false., 'b'), &
                             & foreign(complex_stuff_array(3), fp_stuff_array(3), int_stuff_array(3),  .true., 'c'), &
                             & foreign(complex_stuff_array(4), fp_stuff_array(4), int_stuff_array(4), .false., 'd'), &
                             & foreign(complex_stuff_array(5), fp_stuff_array(5), int_stuff_array(5),  .true., 'e')/))
  call verify_foreign

  call record_foreign(size(foreign_array), &
                            (/ foreign:: (foreign(complex_stuff_array(i), fp_stuff_array(i), int_stuff_array(i), mod(i-1,2)==0, achar(iachar('a')+i-1)), i=1,5) /))
  call verify_foreign

  if (errorsFound /= 0) then
     print *, "Total errors found: ", errorsFound
     stop 4
  end if

contains

  logical function float_not_same(a, b)
    real(C_FLOAT), intent(in) :: a, b
    logical(4) :: precision_r4
    float_not_same = .not. ((a == b) .or. (ieee_is_nan(a) .and. ieee_is_nan(b)) &
         & .or. (ieee_is_finite(a) .and. ieee_is_finite(b) .and. precision_r4(a,b)))
  end function float_not_same

  logical function double_not_same(a, b)
    real(C_DOUBLE), intent(in) :: a, b
    logical(4) :: precision_r8
    double_not_same = .not.((a == b) .or. (ieee_is_nan(a) .and. ieee_is_nan(b)) &
         & .or. (ieee_is_finite(a) .and. ieee_is_finite(b) .and. precision_r8(a,b)))
  end function double_not_same

  logical function long_double_not_same(a, b)
    real(C_LONG_DOUBLE), intent(in) :: a, b
    logical(4) :: precision_r16
    long_double_not_same = .not. ((a == b) .or. (ieee_is_nan(a) .and. ieee_is_nan(b)) &
         & .or. (ieee_is_finite(a) .and. ieee_is_finite(b) .and. precision_r16(a,b)))
  end function long_double_not_same

  logical function float_complex_not_same(a, b)
    complex(C_FLOAT_COMPLEX), intent(in) :: a, b
    float_complex_not_same = float_not_same(real(a),real(b)) .or. float_not_same(aimag(a),aimag(b))
  end function float_complex_not_same

  logical function double_complex_not_same(a, b)
    complex(C_DOUBLE_COMPLEX), intent(in) :: a, b
    double_complex_not_same = double_not_same(real(a),real(b)) .or. double_not_same(aimag(a),aimag(b))
  end function double_complex_not_same

  logical function long_double_complex_not_same(a, b)
    complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a, b
    long_double_complex_not_same = long_double_not_same(real(a),real(b)) .or. long_double_not_same(aimag(a),aimag(b))
  end function long_double_complex_not_same

  logical function complex_stuff_not_same(a, b)
    type (complex_stuff), intent(in) :: a, b
    complex_stuff_not_same = long_double_complex_not_same(a%long_double_complex_field, b%long_double_complex_field) &
         &             .or. double_complex_not_same(a%double_complex_field, b%double_complex_field) &
         &             .or. float_complex_not_same(a%float_complex_field, b%float_complex_field)
  end function complex_stuff_not_same

  logical function fp_stuff_not_same(a, b)
    type (fp_stuff), intent(in) :: a, b
    fp_stuff_not_same = long_double_not_same(a%long_double_field, b%long_double_field) &
         &             .or. double_not_same(a%double_field, b%double_field) &
         &             .or. float_not_same(a%float_field, b%float_field)
  end function fp_stuff_not_same

  logical function int_stuff_not_same(a, b)
    type (int_stuff), intent(in) :: a, b
    int_stuff_not_same = (a%long_long_field /= b%long_long_field) .or. (a%long_field /= b%long_field) &
         &             .or. (a%int_field /= b%int_field) .or. (a%short_field /= b%short_field) &
         &             .or. (a%signed_char_field /= b%signed_char_field)
  end function int_stuff_not_same

  logical function foreign_not_same(a, b)
    type (foreign), intent(in) :: a, b
    foreign_not_same = complex_stuff_not_same(a%complex_stuff_field, b%complex_stuff_field) &
         &             .or. fp_stuff_not_same(a%fp_stuff_field, b%fp_stuff_field) &
         &             .or. int_stuff_not_same(a%int_stuff_field, b%int_stuff_field) &
         &             .or. (a%bool_field .neqv. b%bool_field) &
         &             .or. (a%char_field /= b%char_field)
  end function foreign_not_same

  subroutine verify_int_stuff
    type (int_stuff) :: item
    integer :: i
    do i=1,size(int_stuff_array)
       if (int_stuff_not_same(int_stuff_array(i), get_int_stuff(i))) then
          print *, "Int stuff mismatch:", int_stuff_array(i), "/=", get_int_stuff(i)
          errorsFound = errorsFound + 1
       end if
    end do
  end subroutine verify_int_stuff


  subroutine verify_fp_stuff
    integer :: i
    do i=1,size(fp_stuff_array)
       if (fp_stuff_not_same(fp_stuff_array(i), get_fp_stuff(i))) then
          print *, "Fp stuff mismatch:", fp_stuff_array(i), "/=", get_fp_stuff(i)
          errorsFound = errorsFound + 1
       end if
    end do
  end subroutine verify_fp_stuff


  subroutine verify_complex_stuff
    integer :: i
    do i=1,size(complex_stuff_array)
       if (complex_stuff_not_same(complex_stuff_array(i), get_complex_stuff(i))) then
          print *, "Complex stuff mismatch:", complex_stuff_array(i), "/=", get_complex_stuff(i)
          errorsFound = errorsFound + 1
       end if
    end do
  end subroutine verify_complex_stuff


  subroutine verify_foreign
    integer :: i
    do i=1,size(foreign_array)
       if (foreign_not_same(foreign_array(i), get_foreign(i))) then
          print *, "Foreign stuff mismatch:", foreign_array(i), "/=", get_foreign(i)
          errorsFound = errorsFound + 1
       end if
    end do
  end subroutine verify_foreign

end program acemc15
