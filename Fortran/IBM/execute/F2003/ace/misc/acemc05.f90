!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc05
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: derived type values from C functions
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

program acemc05
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  type, bind(c) :: complex_stuff
    complex(C_LONG_DOUBLE_COMPLEX) :: long_double_complex_field
    complex(C_DOUBLE_COMPLEX)      :: double_complex_field
    complex(C_FLOAT_COMPLEX)       :: float_complex_field
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

     subroutine init_arrays() bind (c)
     end subroutine init_arrays

     type (complex_stuff) function get_complex_stuff(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import complex_stuff
       integer (C_INT), value :: inx
     end function get_complex_stuff


     type (fp_stuff) function get_fp_stuff(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import fp_stuff
       integer (C_INT), value :: inx
     end function get_fp_stuff


     type (int_stuff) function get_int_stuff(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import int_stuff
       integer(C_INT), value       :: inx
     end function get_int_stuff


     type (foreign) function get_foreign(inx) bind (c)
       use, intrinsic :: iso_c_binding
       import foreign
       integer(C_INT), value :: inx
     end function get_foreign

  end interface

  type (int_stuff) :: int_stuff_item, int_stuff_array(5), local_int_stuff_array(5)
  type (fp_stuff) :: fp_stuff_item, fp_stuff_array(5), local_fp_stuff_array(5)
  type (complex_stuff) :: complex_stuff_item, complex_stuff_array(5), local_complex_stuff_array(5)
  type (foreign) :: foreign_item, foreign_array(5), local_foreign_array(5)
  integer :: i
  integer :: bool_size, cdbl_size, cfloat_size, char_size, cldbl_size, dbl_size, &
       float_size, int_size, ldbl_size, llong_size, long_size, schar_size, short_size
  external :: bool_size, cdbl_size, cfloat_size, char_size, cldbl_size, dbl_size, &
       float_size, int_size, ldbl_size, llong_size, long_size, schar_size, short_size

  ! certain routines invoke precision_r4 on real(C_FLOAT) data, etc.,
  ! so they won't work if the size is incorrect, meaning the test is then invalid:
  if (C_FLOAT /= 4 .or. C_DOUBLE /= 8 .or. C_LONG_DOUBLE /= 16) error stop 40_4

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
     error stop 41_4
  end if

  call init_arrays

  int_stuff_array = [int_stuff:: get_int_stuff(1), get_int_stuff(2), get_int_stuff(3), get_int_stuff(4), get_int_stuff(5)]
  local_int_stuff_array = (/ int_stuff(9223372036854775807_C_LONG_LONG, 2147483647_C_LONG, 32767_C_INT, 32767_C_SHORT, 127_C_SIGNED_CHAR), &
                        &    int_stuff(-9223372036854775807_C_LONG_LONG, -2147483647_C_LONG, -32767_C_INT, -32767_C_SHORT, -127_C_SIGNED_CHAR), &
                        &    int_stuff(-1_C_LONG_LONG, -1_C_LONG, -1_C_INT, -1_C_SHORT, -1_C_SIGNED_CHAR), &
                        &    int_stuff( 0_C_LONG_LONG,  0_C_LONG,  0_C_INT,  0_C_SHORT,  0_C_SIGNED_CHAR), &
                        &    int_stuff( 1_C_LONG_LONG,  1_C_LONG,  1_C_INT,  1_C_SHORT,  1_C_SIGNED_CHAR)/)

  if (.not. all_int_stuff_is_same(int_stuff_array, local_int_stuff_array)) error stop 1_4
  if (.not. all_int_stuff_is_same([int_stuff:: get_int_stuff(1), get_int_stuff(2), get_int_stuff(3), get_int_stuff(4), get_int_stuff(5)], &
                                  local_int_stuff_array)) error stop 2_4
  if (.not. all_int_stuff_is_same([get_int_stuff(1), get_int_stuff(2), get_int_stuff(3), get_int_stuff(4), get_int_stuff(5)], &
                                  local_int_stuff_array)) error stop 3_4
  if (.not. all_int_stuff_is_same([(get_int_stuff(i), i=1,5)], local_int_stuff_array)) error stop 4_4

  int_stuff_array = [int_stuff:: (get_int_stuff(i), i=1,5)]
  if (.not. all_int_stuff_is_same(int_stuff_array, local_int_stuff_array)) error stop 5_4

  int_stuff_array = [(get_int_stuff(i), i=1,5)]
  if (.not. all_int_stuff_is_same(int_stuff_array, local_int_stuff_array)) error stop 6_4

  fp_stuff_array = [fp_stuff:: get_fp_stuff(1), get_fp_stuff(2), get_fp_stuff(3), get_fp_stuff(4), get_fp_stuff(5)]
  local_fp_stuff_array = (/ fp_stuff(-9e-100_C_LONG_DOUBLE,-8.0e30_C_DOUBLE,-7.0e20_C_FLOAT), &
		       fp_stuff(-0.0_C_LONG_DOUBLE, -0.0_C_DOUBLE, -0.0_C_FLOAT), &
		       fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF), ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF)), &
		       fp_stuff(9.9e200_C_LONG_DOUBLE, 2.1e-30_C_DOUBLE, 0.3e-9_C_FLOAT), &
		       fp_stuff(ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN), ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN))/)

  if (.not. all_fp_stuff_is_same(fp_stuff_array, local_fp_stuff_array)) error stop 11_4
  if (.not. all_fp_stuff_is_same([fp_stuff:: get_fp_stuff(1), get_fp_stuff(2), get_fp_stuff(3), get_fp_stuff(4), get_fp_stuff(5)], &
                                  local_fp_stuff_array)) error stop 12_4
  if (.not. all_fp_stuff_is_same([get_fp_stuff(1), get_fp_stuff(2), get_fp_stuff(3), get_fp_stuff(4), get_fp_stuff(5)], &
                                  local_fp_stuff_array)) error stop 13_4
  if (.not. all_fp_stuff_is_same([(get_fp_stuff(i), i=1,5)], local_fp_stuff_array)) error stop 14_4

  fp_stuff_array = [fp_stuff:: (get_fp_stuff(i), i=1,5)]
  if (.not. all_fp_stuff_is_same(fp_stuff_array, local_fp_stuff_array)) error stop 15_4

  fp_stuff_array = [(get_fp_stuff(i), i=1,5)]
  if (.not. all_fp_stuff_is_same(fp_stuff_array, local_fp_stuff_array)) error stop 16_4


  complex_stuff_array = [complex_stuff:: get_complex_stuff(1), get_complex_stuff(2), get_complex_stuff(3), get_complex_stuff(4), get_complex_stuff(5)]
  local_complex_stuff_array = (/ complex_stuff((1e1_C_LONG_DOUBLE,2e2_C_LONG_DOUBLE), (3.0_C_DOUBLE,4.0_C_DOUBLE), (0.5_C_FLOAT,0.6_C_FLOAT)), &
       complex_stuff((-1.0_C_LONG_DOUBLE,2.0_C_LONG_DOUBLE), (3.0_C_DOUBLE,-4.0_C_DOUBLE), (-5.0_C_FLOAT,-6.0_C_FLOAT)), &
       complex_stuff((1e200_C_LONG_DOUBLE,2e-200_C_LONG_DOUBLE), (3.0e37_C_DOUBLE,4.0e-37_C_DOUBLE), (0.5e10_C_FLOAT,0.6e-10_C_FLOAT)), &
       complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_POSITIVE_INF)), &
                     (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_POSITIVE_INF)), &
                     (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_POSITIVE_INF))), &
       complex_stuff((ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_LONG_DOUBLE,IEEE_QUIET_NAN)), &
                     (ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN),ieee_value(0.0_C_DOUBLE,IEEE_QUIET_NAN)), &
                     (ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN),ieee_value(0.0_C_FLOAT,IEEE_QUIET_NAN)))/)

  if (.not. all_complex_stuff_is_same(complex_stuff_array, local_complex_stuff_array)) error stop 21_4
  if (.not. all_complex_stuff_is_same([complex_stuff:: get_complex_stuff(1), get_complex_stuff(2), get_complex_stuff(3), get_complex_stuff(4), get_complex_stuff(5)], &
                                  local_complex_stuff_array)) error stop 22_4
  if (.not. all_complex_stuff_is_same([get_complex_stuff(1), get_complex_stuff(2), get_complex_stuff(3), get_complex_stuff(4), get_complex_stuff(5)], &
                                  local_complex_stuff_array)) error stop 23_4
  if (.not. all_complex_stuff_is_same([(get_complex_stuff(i), i=1,5)], local_complex_stuff_array)) error stop 24_4

  complex_stuff_array = [complex_stuff:: (get_complex_stuff(i), i=1,5)]
  if (.not. all_complex_stuff_is_same(complex_stuff_array, local_complex_stuff_array)) error stop 25_4

  complex_stuff_array = [(get_complex_stuff(i), i=1,5)]
  if (.not. all_complex_stuff_is_same(complex_stuff_array, local_complex_stuff_array)) error stop 26_4


  foreign_array = [foreign:: get_foreign(1), get_foreign(2), get_foreign(3), get_foreign(4), get_foreign(5)]
  local_foreign_array = (/ (foreign(complex_stuff_array(i), fp_stuff_array(i), int_stuff_array(i), mod(i-1,2)==0, achar(iachar('a')+i-1)), i=1,5) /)

  if (.not. all_foreign_is_same(foreign_array, local_foreign_array)) error stop 31_4
  if (.not. all_foreign_is_same([foreign:: get_foreign(1), get_foreign(2), get_foreign(3), get_foreign(4), get_foreign(5)], &
                                  local_foreign_array)) error stop 32_4
  if (.not. all_foreign_is_same([get_foreign(1), get_foreign(2), get_foreign(3), get_foreign(4), get_foreign(5)], &
                                  local_foreign_array)) error stop 33_4
  if (.not. all_foreign_is_same([(get_foreign(i), i=1,5)], local_foreign_array)) error stop 34_4

  foreign_array = [foreign:: (get_foreign(i), i=1,5)]
  if (.not. all_foreign_is_same(foreign_array, local_foreign_array)) error stop 35_4

  foreign_array = [(get_foreign(i), i=1,5)]
  if (.not. all_foreign_is_same(foreign_array, local_foreign_array)) error stop 36_4

contains

  logical function float_is_same(a, b)
    real(C_FLOAT), intent(in) :: a, b
    logical(4) :: precision_r4
    float_is_same = (a == b) .or. (ieee_is_nan(a) .and. ieee_is_nan(b)) &
         & .or. (ieee_is_finite(a) .and. ieee_is_finite(b) .and. precision_r4(a,b))
  end function float_is_same

  logical function double_is_same(a, b)
    real(C_DOUBLE), intent(in) :: a, b
    logical(4) :: precision_r8
    double_is_same = (a == b) .or. (ieee_is_nan(a) .and. ieee_is_nan(b)) &
         & .or. (ieee_is_finite(a) .and. ieee_is_finite(b) .and. precision_r8(a,b))
  end function double_is_same

  logical function long_double_is_same(a, b)
    real(C_LONG_DOUBLE), intent(in) :: a, b
    logical(4) :: precision_r16
    long_double_is_same = (a == b) .or. (ieee_is_nan(a) .and. ieee_is_nan(b)) &
         & .or. (ieee_is_finite(a) .and. ieee_is_finite(b) .and. precision_r16(a,b))
  end function long_double_is_same

  logical function float_complex_is_same(a, b)
    complex(C_FLOAT_COMPLEX), intent(in) :: a, b
    float_complex_is_same = float_is_same(real(a),real(b)) .and. float_is_same(aimag(a),aimag(b))
  end function float_complex_is_same

  logical function double_complex_is_same(a, b)
    complex(C_DOUBLE_COMPLEX), intent(in) :: a, b
    double_complex_is_same = double_is_same(real(a),real(b)) .and. double_is_same(aimag(a),aimag(b))
  end function double_complex_is_same

  logical function long_double_complex_is_same(a, b)
    complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a, b
    long_double_complex_is_same = long_double_is_same(real(a),real(b)) .and. long_double_is_same(aimag(a),aimag(b))
  end function long_double_complex_is_same

  logical function complex_stuff_is_same(a, b)
    type (complex_stuff), intent(in) :: a, b
    complex_stuff_is_same = long_double_complex_is_same(a%long_double_complex_field, b%long_double_complex_field) &
         &             .and. double_complex_is_same(a%double_complex_field, b%double_complex_field) &
         &             .and. float_complex_is_same(a%float_complex_field, b%float_complex_field)
    if (.not. complex_stuff_is_same) print *, "Complex_stuff", a, "/=", b
  end function complex_stuff_is_same

  logical function fp_stuff_is_same(a, b)
    type (fp_stuff), intent(in) :: a, b
    fp_stuff_is_same = long_double_is_same(a%long_double_field, b%long_double_field) &
         &             .and. double_is_same(a%double_field, b%double_field) &
         &             .and. float_is_same(a%float_field, b%float_field)
    if (.not. fp_stuff_is_same) print *, "Fp_stuff", a, "/=", b
  end function fp_stuff_is_same

  logical function int_stuff_is_same(a, b)
    type (int_stuff), intent(in) :: a, b
    int_stuff_is_same = (a%long_long_field == b%long_long_field) .and. (a%long_field == b%long_field) &
         &             .and. (a%int_field == b%int_field) .and. (a%short_field == b%short_field) &
         &             .and. (a%signed_char_field == b%signed_char_field)
    if (.not. int_stuff_is_same) print *, "Int_stuff", a, "/=", b
  end function int_stuff_is_same

  logical function foreign_is_same(a, b)
    type (foreign), intent(in) :: a, b
    foreign_is_same = complex_stuff_is_same(a%complex_stuff_field, b%complex_stuff_field) &
         &             .and. fp_stuff_is_same(a%fp_stuff_field, b%fp_stuff_field) &
         &             .and. int_stuff_is_same(a%int_stuff_field, b%int_stuff_field) &
         &             .and. (a%bool_field .eqv. b%bool_field) &
         &             .and. (a%char_field == b%char_field)
    if (.not. foreign_IS_SAME) print *, "Foreign", a, "/=", b
  end function foreign_is_same

  logical function all_complex_stuff_is_same(a, b)
    type (complex_stuff), intent(in) :: a(:), b(:)
    integer :: i
    all_complex_stuff_is_same = size(a) == size(b) .and. all([(complex_stuff_is_same(a(i),b(i)),i=1,size(a))])
  end function all_complex_stuff_is_same

  logical function all_fp_stuff_is_same(a, b)
    type (fp_stuff), intent(in) :: a(:), b(:)
    integer :: i
    all_fp_stuff_is_same = size(a) == size(b) .and. all([(fp_stuff_is_same(a(i),b(i)),i=1,size(a))])
  end function all_fp_stuff_is_same

  logical function all_int_stuff_is_same(a, b)
    type (int_stuff), intent(in) :: a(:), b(:)
    integer :: i
    all_int_stuff_is_same = size(a) == size(b) .and. all([(int_stuff_is_same(a(i),b(i)),i=1,size(a))])
  end function all_int_stuff_is_same

  logical function all_foreign_is_same(a, b)
    type (foreign), intent(in) :: a(:), b(:)
    integer :: i
    all_foreign_is_same = size(a) == size(b) .and. all([(foreign_is_same(a(i),b(i)),i=1,size(a))])
  end function all_foreign_is_same

end program acemc05
