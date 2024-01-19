!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-07-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values to C functions (logical + character)
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
!*  arrays.  The test is successful (return code of 0) if the values are correct
!*  and the selftest shows that errors would be detected if they occur.
!*  (The selftest works by intentionally passing the wrong values to C, and
!*  verifying that the error is detected; it is always executed, so there
!*  will always be some diagnostic output from this test.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc14

  use, intrinsic :: iso_c_binding
  implicit none

  interface

     integer(C_INT) function bool_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function bool_size

     integer(C_INT) function char_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function char_size


     subroutine record_chars(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       character(C_CHAR) :: arr(*)
     end subroutine record_chars

     character(C_CHAR) function get_char(i) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: i
     end function get_char

     subroutine record_bools(l, arr) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: l
       logical(C_BOOL) :: arr(*)
     end subroutine record_bools

     logical(C_BOOL) function get_bool(i) bind (c)
       use, intrinsic :: iso_c_binding
       integer(C_INT), value :: i
     end function get_bool

  end interface

  character(C_CHAR) :: charr(8)
  logical(C_BOOL)   :: larr(4)
  integer :: i
  integer(4) :: errorsFound

  errorsFound = 0

  if (char_size() /= C_CHAR .or. bool_size() /= C_BOOL) then
     print *, "Incorrect char/bool size:"
     print *, " bool size is ", bool_size(), " should be ", C_BOOL
     print *, " char size is ", char_size(), " should be ", C_CHAR
     error stop 2_4
  end if

  charr = [' ', '0', '9', 'A', 'Z', 'a', 'z', '~']
  larr = [.false._C_BOOL, .true._C_BOOL, logical(charr(1)==charr(2),C_BOOL), logical(charr(1)==charr(1),C_BOOL)]

  call record_chars(size(charr), charr)
  call verify_char_array
  call record_chars(size(charr), [ ' ', '0', '9', 'A', 'Z', 'a', 'z', '~' ])
  call verify_char_array
  call record_chars(size(charr), [ (' ', '0', '9', 'A', 'Z', 'a', 'z', '~', i=1,size(charr)) ])
  call verify_char_array
  call record_chars(size(charr), [character(C_CHAR):: ' ', '0', '9', 'A', 'Z', 'a', 'z', '~' ])
  call verify_char_array
  call record_chars(size(charr), [character(C_CHAR):: (' ', '0', '9', 'A', 'Z', 'a', 'z', '~', i=1,size(charr)) ])
  call verify_char_array


  call record_bools(size(larr), larr)
  call verify_bool_array
  call record_bools(size(larr), [.false._C_BOOL, .true._C_BOOL, logical(charr(1)==charr(2),C_BOOL), logical(charr(1)==charr(1),C_BOOL)])
  call verify_bool_array
  call record_bools(size(larr), [(.false._C_BOOL, .true._C_BOOL, logical(charr(1)==charr(2),C_BOOL), logical(charr(1)==charr(1),C_BOOL), i=1,size(larr))])
  call verify_bool_array
  call record_bools(size(larr), [logical(C_BOOL):: .false._C_BOOL, .true._C_BOOL, logical(charr(1)==charr(2),C_BOOL), logical(charr(1)==charr(1),C_BOOL)])
  call verify_bool_array
  call record_bools(size(larr), [logical(C_BOOL):: (.false._C_BOOL, .true._C_BOOL, logical(charr(1)==charr(2),C_BOOL), logical(charr(1)==charr(1),C_BOOL), i=1,size(larr)) ])
  call verify_bool_array

  if (errorsFound /= 0) call zzrc(10_4 + errorsFound)

  !! Now perform a self-test: make sure that we can detect the apparently incorrect recording of values:
  print *, "Self-test: expect 4 mismatches (2 character, 2 logical):"
  call record_chars(size(charr), ['!', '0', '9', 'A', 'Z', 'a', 'z', '}' ])
  call verify_char_array
  call record_bools(size(larr), [logical(C_BOOL):: .true._C_BOOL, .true._C_BOOL, .false._C_BOOL, .false._C_BOOL])
  call verify_bool_array

  if (errorsFound /= 4) error stop 4_4

contains

  subroutine verify_char_array
    do i=1,size(charr)
       if (charr(i) /= get_char(i)) then
          print *, "Char mismatch:", charr(i), "/=", get_char(i)
          errorsFound = errorsFound + 1
       end if
    end do
  end subroutine verify_char_array

  subroutine verify_bool_array
    do i=1,size(larr)
       if (larr(i) .neqv. get_bool(i)) then
          print *, "Bool mismatch:", larr(i), "/=", get_bool(i)
          errorsFound = errorsFound + 1
       end if
    end do
  end subroutine verify_bool_array

end program acemc14
