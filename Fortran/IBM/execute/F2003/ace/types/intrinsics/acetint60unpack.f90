!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint60unpack
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics - unpack
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use array construction functions not used elsewhere.  We're already using
!*  RESHAPE in many places, but no others (MERGE has been used, but not in its
!*  array form).  Here we test:
!*     UNPACK    (vector, mask, field)           inverse of pack
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint60unpack

  implicit none
  integer :: array(9), outcome(3,3), i
  logical, parameter :: T = .true., F = .false.
  logical :: mask (3,3)

  mask = reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3])

  ! T . .
  ! T T .
  ! . T T

  array = [integer:: (i, i=1,9)]
  print *, unpack(array, mask, 999) ! 1 2 999 999 3 4 999 999 5
  array = [integer:: unpack(array, mask, 999)]
  if (any(array /= [integer:: 1, 2, 999, 999, 3, 4, 999, 999, 5])) stop 2

  print *, unpack([integer:: 1,2,3,4,5,6,7,8,9], [logical:: T, T, F, F, T, T, F, F, T], 999)
  array = unpack([integer:: 1,2,3,4,5,6,7,8,9], [logical:: T, T, F, F, T, T, F, F, T], 999)
  if (any(array /= [integer:: 1, 2, 999, 999, 3, 4, 999, 999, 5])) stop 3

  array = [integer:: unpack([integer:: 1,2,3,4,5,6,7,8,9], [logical:: T, T, F, F, T, T, F, F, T], 999)]
  if (any(array /= [integer:: 1, 2, 999, 999, 3, 4, 999, 999, 5])) stop 4

  print *, unpack([integer:: 1,2,3,4,5,6,7,8,9], reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3]), 999)
  array = [integer:: unpack([integer:: 1,2,3,4,5,6,7,8,9], reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3]), 999)]
  if (any(array /= [integer:: 1, 2, 999, 999, 3, 4, 999, 999, 5])) stop 5

  array = unpack([integer:: 1,2,3,4,5,6,7,8,9], [logical:: T, T, F, F, T, T, F, F, T], [integer:: 81, 64, 49, 36, 25, 16, 9, 4, 1])
  if (any(array /= [integer:: 1, 2, 49, 36, 3, 4, 9, 4, 5])) stop 6

  outcome = unpack([integer:: 1,2,3,4,5], reshape([logical:: T, T, F, F, T, T, F, F, T], [3,3]), reshape([integer:: 81, 64, 49, 36, 25, 16, 9, 4, 1],[3,3]))
  if (any([integer:: outcome] /= [integer:: 1, 2, 49, 36, 3, 4, 9, 4, 5])) stop 7

end program acetint60unpack
