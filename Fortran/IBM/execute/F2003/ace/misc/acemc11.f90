!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc11
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-21
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values to C functions (integer)
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

program acemc11

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

     subroutine record_ints(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       integer(C_INT) :: arr(*)
     end subroutine record_ints

     integer(C_INT) function get_int(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_int

     subroutine record_long_ints(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       integer(C_LONG) :: arr(*)
     end subroutine record_long_ints

     integer(C_LONG) function get_long_int(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_long_int

     subroutine record_long_long_ints(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       integer(C_LONG_LONG) :: arr(*)
     end subroutine record_long_long_ints

     integer(C_LONG_LONG) function get_long_long_int(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_long_long_int

     subroutine record_signed_chars(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       integer(C_SIGNED_CHAR) :: arr(*)
     end subroutine record_signed_chars

     integer(C_SIGNED_CHAR) function get_signed_char(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_signed_char

     subroutine record_short_ints(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       integer(C_SHORT) :: arr(*)
     end subroutine record_short_ints

     integer(C_SHORT) function get_short_int(inx) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: inx
     end function get_short_int

  end interface

  integer(C_SIGNED_CHAR) :: carr(5)
  integer(C_SHORT)       :: sharr(5)
  integer(C_INT)         :: iarr(5)
  integer(C_LONG)        :: larr(5)
  integer(C_LONG_LONG)   :: llarr(5)
  integer(C_INT)         :: i
  integer(4) :: errorsFound

  errorsFound = 0

  if (schar_size() /= C_SIGNED_CHAR .or. short_size() /= C_SHORT .or. int_size() /= C_INT &
       .or. long_size() /= C_LONG .or. llong_size() /= C_LONG_LONG) then
     print *, "Incorrect int/short/long/long long/signed char size:"
     print *, " int size is ", int_size(), " should be ", C_INT
     print *, " short size is ", short_size(), " should be ", C_SHORT
     print *, " (signed) char size is ", schar_size(), " should be ", C_SIGNED_CHAR
     print *, " long size is ", long_size(), " should be ", C_LONG
     print *, " long long size is ", llong_size(), " should be ", C_LONG_LONG
     error stop 2_4
  end if

  carr = [ -huge(carr(1)), -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, huge(carr(1)) ]
  sharr = [ -huge(sharr(1)), -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, huge(sharr(1)) ]
  iarr = [ -huge(iarr(1)), -1_C_INT, 0_C_INT, 1_C_INT, huge(iarr(1)) ]
  larr = [ -huge(larr(1)), -1_C_LONG, 0_C_LONG, 1_C_LONG, huge(larr(1)) ]
  llarr = [ -huge(llarr(1)), -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, huge(llarr(1)) ]

  call record_signed_chars(size(carr), carr)
  call verify_signed_chars
  call record_signed_chars(size(carr), [ -huge(carr(1)), -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, huge(carr(1)) ])
  call verify_signed_chars
  call record_signed_chars(size(carr), [ (-huge(carr(1)), -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, huge(carr(1)), i=1,1) ])
  call verify_signed_chars
  call record_signed_chars(size(carr), [integer(C_SIGNED_CHAR):: -huge(carr(1)), -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, huge(carr(1)) ])
  call verify_signed_chars
  call record_signed_chars(size(carr), [integer(C_SIGNED_CHAR):: (-huge(carr(1)), -1_C_SIGNED_CHAR, 0_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, huge(carr(1)), i=1,1) ])
  call verify_signed_chars

  call record_short_ints(size(sharr), sharr)
  call verify_short_ints
  call record_short_ints(size(sharr), [ -huge(sharr(1)), -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, huge(sharr(1)) ])
  call verify_short_ints
  call record_short_ints(size(sharr), [ (-huge(sharr(1)), -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, huge(sharr(1)), i=1,1) ])
  call verify_short_ints
  call record_short_ints(size(sharr), [integer(C_SHORT):: -huge(sharr(1)), -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, huge(sharr(1)) ])
  call verify_short_ints
  call record_short_ints(size(sharr), [integer(C_SHORT):: (-huge(sharr(1)), -1_C_SHORT, 0_C_SHORT, 1_C_SHORT, huge(sharr(1)), i=1,1) ])
  call verify_short_ints

  call record_ints(size(iarr), iarr)
  call verify_ints
  call record_ints(size(iarr), [ -huge(iarr(1)), -1_C_INT, 0_C_INT, 1_C_INT, huge(iarr(1)) ])
  call verify_ints
  call record_ints(size(iarr), [ (-huge(iarr(1)), -1_C_INT, 0_C_INT, 1_C_INT, huge(iarr(1)), i=1,1) ])
  call verify_ints
  call record_ints(size(iarr), [integer(C_INT):: -huge(iarr(1)), -1_C_INT, 0_C_INT, 1_C_INT, huge(iarr(1)) ])
  call verify_ints
  call record_ints(size(iarr), [integer(C_INT):: (-huge(iarr(1)), -1_C_INT, 0_C_INT, 1_C_INT, huge(iarr(1)), i=1,1) ])
  call verify_ints

  call record_long_ints(size(larr), larr)
  call verify_long_ints
  call record_long_ints(size(larr), [ -huge(larr(1)), -1_C_LONG, 0_C_LONG, 1_C_LONG, huge(larr(1)) ])
  call verify_long_ints
  call record_long_ints(size(larr), [ (-huge(larr(1)), -1_C_LONG, 0_C_LONG, 1_C_LONG, huge(larr(1)), i=1,1) ])
  call verify_long_ints
  call record_long_ints(size(larr), [integer(C_LONG):: -huge(larr(1)), -1_C_LONG, 0_C_LONG, 1_C_LONG, huge(larr(1)) ])
  call verify_long_ints
  call record_long_ints(size(larr), [integer(C_LONG):: (-huge(larr(1)), -1_C_LONG, 0_C_LONG, 1_C_LONG, huge(larr(1)), i=1,1) ])
  call verify_long_ints

  call record_long_long_ints(size(llarr), llarr)
  call verify_long_long_ints
  call record_long_long_ints(size(llarr), [ -huge(llarr(1)), -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, huge(llarr(1)) ])
  call verify_long_long_ints
  call record_long_long_ints(size(llarr), [ (-huge(llarr(1)), -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, huge(llarr(1)), i=1,1) ])
  call verify_long_long_ints
  call record_long_long_ints(size(llarr), [integer(C_LONG_LONG):: -huge(llarr(1)), -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, huge(llarr(1)) ])
  call verify_long_long_ints
  call record_long_long_ints(size(llarr), [integer(C_LONG_LONG):: (-huge(llarr(1)), -1_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, huge(llarr(1)), i=1,1) ])
  call verify_long_long_ints

  if (errorsFound /= 0) call zzrc (10_4 + errorsFound)

  !! Now perform a self-test: make sure that we can detect the apparently incorrect recording of values:
  print *, "Self-test: expect 5 mismatches (long long, long, int, short, signed char):"

  call record_long_long_ints(size(llarr), [integer(C_LONG_LONG):: (-huge(llarr(1)), -2_C_LONG_LONG, 0_C_LONG_LONG, 1_C_LONG_LONG, huge(llarr(1)), i=1,1) ])
  call verify_long_long_ints
  call record_long_ints(size(larr), [integer(C_LONG):: (-huge(larr(1)), -1_C_LONG, 1_C_LONG, 1_C_LONG, huge(larr(1)), i=1,1) ])
  call verify_long_ints
  call record_ints(size(iarr), [integer(C_INT):: (-huge(iarr(1)), -1_C_INT, 0_C_INT, 3_C_INT, huge(iarr(1)), i=1,1) ])
  call verify_ints
  call record_short_ints(size(sharr), [integer(C_SHORT):: (-huge(sharr(1)), -3_C_SHORT, 0_C_SHORT, 1_C_SHORT, huge(sharr(1)), i=1,1) ])
  call verify_short_ints
  call record_signed_chars(size(carr), [integer(C_SIGNED_CHAR):: (-huge(carr(1)), -1_C_SIGNED_CHAR, 4_C_SIGNED_CHAR, 1_C_SIGNED_CHAR, huge(carr(1)), i=1,1) ])
  call verify_signed_chars

  if (errorsFound /= 5) error stop 4_4


contains

  subroutine verify_ints
    integer :: i
    do i=1,size(iarr)
       if (iarr(i) /= get_int(i)) then
          errorsFound = errorsFound + 1
          print *, "Int mismatch:", iarr(i), "/=", get_int(i)
       end if
    end do
  end subroutine verify_ints

  subroutine verify_long_ints
    integer :: i
    do i=1,size(larr)
       if (larr(i) /= get_long_int(i)) then
          errorsFound = errorsFound + 1
          print *, "Long int mismatch:", larr(i), "/=", get_long_int(i)
       end if
    end do
  end subroutine verify_long_ints

  subroutine verify_long_long_ints
    integer :: i
    do i=1,size(llarr)
       if (llarr(i) /= get_long_long_int(i)) then
          errorsFound = errorsFound + 1
          print *, "Long long int mismatch:", llarr(i), "/=", get_long_long_int(i)
       end if
    end do
  end subroutine verify_long_long_ints

  subroutine verify_signed_chars
    integer :: i
    do i=1,size(carr)
       if (carr(i) /= get_signed_char(i)) then
          errorsFound = errorsFound + 1
          print *, "Signed char mismatch:", carr(i), "/=", get_signed_char(i)
       end if
    end do
  end subroutine verify_signed_chars

  subroutine verify_short_ints
    integer :: i
    do i=1,size(sharr)
       if (sharr(i) /= get_short_int(i)) then
          errorsFound = errorsFound + 1
          print *, "Short int mismatch:", sharr(i), "/=", get_short_int(i)
       end if
    end do
  end subroutine verify_short_ints

end program acemc11
