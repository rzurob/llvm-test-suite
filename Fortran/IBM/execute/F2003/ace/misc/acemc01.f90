!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-21
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values from C functions
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
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc01

  use, intrinsic :: iso_c_binding
  implicit none

  interface

     integer(C_INT) function int_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function int_size

     integer(C_INT) function short_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function short_size

     integer(C_INT) function long_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function long_size

     integer(C_INT) function llong_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function llong_size

     integer(C_INT) function schar_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function schar_size


     function signed_char_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_SIGNED_CHAR) :: signed_char_fun
       integer(C_INT), value :: sel
     end function signed_char_fun

     function short_int_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_SHORT) :: short_int_fun
       integer(C_INT), value :: sel
     end function short_int_fun

     function int_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT) :: int_fun
       integer(C_INT), value :: sel
     end function int_fun

     function long_int_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_LONG) :: long_int_fun
       integer(C_INT), value :: sel
     end function long_int_fun

     function long_long_int_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_LONG_LONG) :: long_long_int_fun
       integer(C_INT), value :: sel
     end function long_long_int_fun

  end interface

  integer(C_SIGNED_CHAR) :: carr(5), carr2(5), carrExp(5)
  integer(C_SHORT)       :: sharr(5), sharr2(5), sharrExp(5)
  integer(C_INT)         :: iarr(5), iarr2(5), iarrExp(5)
  integer(C_LONG)        :: larr(5), larr2(5), larrExp(5)
  integer(C_LONG_LONG)   :: llarr(5), llarr2(5), llarrExp(5)
  integer(C_INT)         :: i

  
  if (schar_size() /= C_SIGNED_CHAR .or. short_size() /= C_SHORT .or. int_size() /= C_INT &
       .or. long_size() /= C_LONG .or. llong_size() /= C_LONG_LONG) then
     print *, "Incorrect int/short/long/long long/signed char size:"
     print *, " int size is ", int_size(), " should be ", C_INT
     print *, " short size is ", short_size(), " should be ", C_SHORT
     print *, " (signed) char size is ", schar_size(), " should be ", C_SIGNED_CHAR
     print *, " long size is ", long_size(), " should be ", C_LONG
     print *, " long long size is ", llong_size(), " should be ", C_LONG_LONG
     error stop 20_4
  end if


  carr    = (/ (signed_char_fun(i),i=0,4) /)
  carr2   = (/  signed_char_fun(0), signed_char_fun(1), signed_char_fun(2), signed_char_fun(3), signed_char_fun(4) /)
  carrExp = (/ -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, 127_C_SIGNED_CHAR, -127_C_SIGNED_CHAR /)

  if (any(carr /= carrExp) .or. any(carr2 /= carrExp)) error stop 1_4

  carr    = (/ integer(C_SIGNED_CHAR):: (signed_char_fun(i),i=0,4) /)
  carr2   = (/ integer(C_SIGNED_CHAR):: signed_char_fun(0), signed_char_fun(1), signed_char_fun(2), signed_char_fun(3), signed_char_fun(4) /)
  carrExp = (/ integer(C_SIGNED_CHAR):: -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, 127_C_SIGNED_CHAR, -127_C_SIGNED_CHAR /)

  if (any(carr /= carrExp) .or. any(carr2 /= carrExp)) error stop 2_4


  sharr    = (/ (short_int_fun(i),i=0,4) /)
  sharr2   = (/  short_int_fun(0), short_int_fun(1), short_int_fun(2), short_int_fun(3), short_int_fun(4) /)
  sharrExp = (/ -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, 32767_C_SHORT, -32767_C_SHORT /)

  if (any(sharr /= sharrExp) .or. any(sharr2 /= sharrExp)) error stop 3_4

  sharr    = (/ integer(C_SHORT):: (short_int_fun(i),i=0,4) /)
  sharr2   = (/ integer(C_SHORT):: short_int_fun(0), short_int_fun(1), short_int_fun(2), short_int_fun(3), short_int_fun(4) /)
  sharrExp = (/ integer(C_SHORT):: -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, 32767_C_SHORT, -32767_C_SHORT /)

  if (any(sharr /= sharrExp) .or. any(sharr2 /= sharrExp)) error stop 4_4


  iarr    = (/ (int_fun(i),i=0,4) /)
  iarr2   = (/  int_fun(0), int_fun(1), int_fun(2), int_fun(3), int_fun(4) /)
  iarrExp = (/ -1_C_INT, 0_C_INT, 1_C_INT, 32767_C_INT, -32767_C_INT /)

  if (any(iarr /= iarrExp) .or. any(iarr2 /= iarrExp)) error stop 5_4

  iarr    = (/ integer(C_INT):: (int_fun(i),i=0,4) /)
  iarr2   = (/ integer(C_INT):: int_fun(0), int_fun(1), int_fun(2), int_fun(3), int_fun(4) /)
  iarrExp = (/ integer(C_INT):: -1_C_INT, 0_C_INT, 1_C_INT, 32767_C_INT, -32767_C_INT /)

  if (any(iarr /= iarrExp) .or. any(iarr2 /= iarrExp)) error stop 6_4


  larr    = (/ (long_int_fun(i),i=0,4) /)
  larr2   = (/  long_int_fun(0), long_int_fun(1), long_int_fun(2), long_int_fun(3), long_int_fun(4) /)
  larrExp = (/ -1_C_LONG, 0_C_LONG, 1_C_LONG, 2147483647_C_LONG, -2147483647_C_LONG /)

  if (any(larr /= larrExp) .or. any(larr2 /= larrExp)) error stop 7_4

  larr    = (/ integer(C_LONG):: (long_int_fun(i),i=0,4) /)
  larr2   = (/ integer(C_LONG):: long_int_fun(0), long_int_fun(1), long_int_fun(2), long_int_fun(3), long_int_fun(4) /)
  larrExp = (/ integer(C_LONG):: -1_C_LONG, 0_C_LONG, 1_C_LONG, 2147483647_C_LONG, -2147483647_C_LONG /)

  if (any(larr /= larrExp) .or. any(larr2 /= larrExp)) error stop 8_4


  llarr    = (/ (long_long_int_fun(i),i=0,4) /)
  llarr2   = (/  long_long_int_fun(0), long_long_int_fun(1), long_long_int_fun(2), &
                                         & long_long_int_fun(3), long_long_int_fun(4) /)
  llarrExp = (/ -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, &
                9223372036854775807_C_LONG_LONG, -9223372036854775807_C_LONG_LONG /)
  

  if (any(llarr /= llarrExp) .or. any(llarr2 /= llarrExp)) error stop 9_4

  llarr    = (/ integer(C_LONG_LONG):: (long_long_int_fun(i),i=0,4) /)
  llarr2   = (/ integer(C_LONG_LONG):: long_long_int_fun(0), long_long_int_fun(1), &
                                     & long_long_int_fun(2), long_long_int_fun(3), long_long_int_fun(4) /)
  llarrExp = (/ integer(C_LONG_LONG):: -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, &
                9223372036854775807_C_LONG_LONG, -9223372036854775807_C_LONG_LONG /)

  if (any(llarr /= llarrExp) .or. any(llarr2 /= llarrExp)) error stop 10_4

end program acemc01
