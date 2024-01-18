!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint60pack
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-24
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
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
!*     PACK      (array, mask [,vector])         Pack into an array of rank one under a mask
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint60pack

  implicit none
  integer :: array(9), i

  array = [integer:: (i, i=1,9)]
  print *, pack(reshape([integer:: 1,4,9,16,25,36,49,64,81], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0) ! 9 36 81
  if (any(pack(reshape([integer:: 1,4,9,16,25,36,49,64,81], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0) &
          /= [integer:: 9, 36, 81])) stop 2
  array = pack(reshape([integer:: 1,4,9,16,25,36,49,64,81], [integer:: 3,3]), &
                       mod(reshape(array,[3,3]),3) == 0, array)
  if (any(array /= [integer:: 9, 36, 81, 4, 5, 6, 7, 8, 9])) stop 3

  ! 9 36 81 216 125 64 27 8 1:
  array = [integer:: (i, i=1,9)]
  print *, pack(reshape([integer:: 1,4,9,16,25,36,49,64,81], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0, &
                [integer:: 729, 512, 343, 216, 125, 64, 27, 8, 1])
  array =  pack(reshape([integer:: 1,4,9,16,25,36,49,64,81], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0, &
                [integer:: 729, 512, 343, 216, 125, 64, 27, 8, 1])
  if (any(array /= [integer:: 9, 36, 81, 216, 125, 64, 27, 8, 1])) stop 4

  print *, pack([integer:: -3,-2,-1,0], .true., [integer:: 5,6,7,8,9,10,11,12,13])
  array =  pack([integer:: -3,-2,-1,0], .true., [integer:: 5,6,7,8,9,10,11,12,13])
  if (any(array /= [integer:: -3, -2, -1, 0, 9, 10, 11, 12, 13])) stop 5


  array = [integer:: (i, i=1,9)]
  print *, pack(reshape([integer:: (i ** 2, i=1,9)], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0) ! 9 36 81
  if (any(pack(reshape([integer:: (i ** 2, i=1,9)], [integer:: 3,3]), &
                mod(reshape(array,[3,3]), 3) == 0) &
          /= [integer:: (i**2, i=3,9,3)])) stop 6
  array = pack(reshape([integer:: (i ** 2, i=1,9)], [integer:: 3,3]), &
                mod(reshape(array,[3,3]), 3) == 0, array)
  if (any(array /= [integer:: (i ** 2, i=3,9,3), (i, i=4,9)])) stop 7

  array = [integer:: (i, i=1,9)]
  print *, pack(reshape([integer:: (i ** 2, i=1,9)], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0, [integer:: (i ** 3, i=9,1,-1)]) ! 9 36 81 216 125 64 27 8 1
  array =  pack(reshape([integer:: (i ** 2, i=1,9)], [integer:: 3,3]), &
                mod(reshape(array,[3,3]),3) == 0, [integer:: (i ** 3, i=9,1,-1)])
  if (any(array /= [integer:: 9, 36, 81, 216, 125, 64, 27, 8, 1])) stop 8

  print *, pack([integer:: (i-4, i=1,4)], .true., [integer:: (i+4, i=1,9)])
  array = pack([integer:: (i-4, i=1,4)], .true., [integer:: (i+4, i=1,9)])
  if (any(array /= [integer:: -3, -2, -1, 0, 9, 10, 11, 12, 13])) stop 9

end program acetint60pack
