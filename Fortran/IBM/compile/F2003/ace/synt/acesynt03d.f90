!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt03d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Use of square brackets - mismatched delimiters,
!*                               (/.../) vs. [...]
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : syntax, square brackets, array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Test for mismatched pairs of array constructor delimiters, e.g., "(/...]"
!*  or "[.../)".
!*
!*  The test is successful if the correct error messages are generated.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program acesynt03d

  implicit none
  integer    :: iarr(4), empty(0), i
  integer, parameter :: iparm = bit_size([100, 200/))
  integer, parameter :: iparm2 = max((/100], [200/))
  integer, parameter :: iparma(2) = (/100, 200]
  integer, parameter :: iparma2(2) = [100, 200/)

  ! Test simplest mismatch in regular assignment statement:
  iarr = [1, 2, 3, 4/)
  iarr = (/1, 2, 3, 4]

  ! Test nested mismatches:
  iarr = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[(/1,2,3,4/)]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
  iarr = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1,2,3,4/)]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
  iarr = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1,2,3,4]]]]]]]]]/)]]]]]]]]]]]]]]]]]]]]]]
  iarr = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1,2,3,4]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]/)
  iarr = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1,2,3,4]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
  iarr = [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[(/1,2,3,4]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
  iarr = [[[[[[[[[[[[[[(/[[[[[[[[[[[[[[[[[1,2,3,4]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
  iarr = (/[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1,2,3,4]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

  ! Lots of mismatches - 65 levels deep, mismatched at every level
  iarr = (/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/[(/1,2,3,4]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]/)]

  ! Mismatch in the presence of type specifiers
  iarr = [complex::[real::[integer::[real(4)::[integer(4)::]]]]]
  iarr = [complex::[real::[integer::[real(4)::(/integer(4)::/)]]]]
  iarr = [complex::[real::[integer::[real(4)::[integer(4)::/)]]]]
  iarr = [complex::[real::[integer::[real(4)::[integer(4)::]]/)]]
  iarr = [complex::[real::[integer::[real(4)::[integer(4)::]]]]/)
  iarr = [complex::[real::[integer::[real(4)::(/integer(4)::]]]]]
  iarr = [complex::[real::(/integer::[real(4)::[integer(4)::]]]]]
  iarr = (/complex::[real::[integer::[real(4)::[integer(4)::]]]]]

  ! Mismatch with a contained implied do
  empty = (/(/(/integer::[real::(i,i=1,0)/)/)/)/)
  empty = (/[integer::(/real::(i,i=1,0)/)/)/)/)
  empty = (/(/(/integer::(/real::(i,i=1,0)]/)/)/)
  empty = (/(/integer::(/real::(i,i=1,0)/)/)/)]

end program acesynt03d
