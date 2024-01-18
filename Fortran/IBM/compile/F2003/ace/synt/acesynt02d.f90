!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt02d
!*
!*  DATE                       : 2006-07-04
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Use of square brackets - missing delimiters
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : syntax, square brackets, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test for missing delimiters (imbalance).
!*
!*  The test is successful if the correct error messages are generated.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program acesynt02d

  implicit none
  integer    :: iarr(4), empty(0), i
  integer, parameter :: iparm = bit_size((/100, (/ 200 /))
  integer, parameter :: iparm2 = max(100, 200/))
  integer, parameter :: iparma(2) = [100, (/ 200
  integer, parameter :: iparma2(2) = (/100/), 200/)

  ! Test simplest imbalances in regular assignment statement:
  iarr = [1, 2, 3, 4
  iarr = (/1, 2, 3, 4
  iarr = 1, 2, 3, 4]
  iarr = 1, 2, 3, 4/)

  ! More involved imbalances in regular assignment statement:
  iarr = [1, [2, [3, [4
  iarr = (/1, [2, (/3, 4
  iarr = (/1, [2, (/3, 4/)
  iarr = (/1, [2, (/3, 4/)]
  iarr = [1, 2, 3], 4]
  iarr = [1, [2], 3], 4]
  iarr = (/1, (/2/), 3/), 4/)

  ! Imbalance in the presence of type specifiers
  empty = [integer::[real::[complex::[real(4)::[integer(4)::
  empty = (/integer::(/real::(/complex::(/real(4)::(/integer(4)::
  empty = integer(4)::/)
  empty = integer(4)::]
  empty = [[integer::[real::]]]]
  empty = [[[integer::[real::]]]
  empty = (/(/integer::(/real::/)/)/)/)
  empty = (/(/(/integer::(/real::/)/)/)

  ! Imbalance with a contained implied do
  empty = (/(/(/integer::(/real::(i,i=1,0)/)/)/)
  empty = (/(/integer::(/real::(i,i=1,0)/)/)/)/)

end program acesynt02d
