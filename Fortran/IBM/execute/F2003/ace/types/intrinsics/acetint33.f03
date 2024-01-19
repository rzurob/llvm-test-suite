!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-09-27
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : intrinsic TS: as selector in ASSOCIATE construct
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
!*  Use AC in an associate construct, verifying that interactions between
!*  associate names, intrinsics, type names, and names of variables in
!*  implied-do's are correct.  Among other things, this means that associate
!*  names cannot be used as ac-do-variables in the body of that associate
!*  construct, and that they trump intrinsics in that context, also.
!*  Associate names can be used as data objects within an AC, however.
!*  Most importantly, however, type specifiers in ACs are not confused by
!*  associate names identical to intrinsic or derived types.
!*
!*  Similarly named tests exist in the intrinsic, derived, and "none" types
!*  sub-buckets, as well as in their diagnostic counterparts.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint33

  implicit none
  integer :: arr(2), arr4(4), arr2(2), iv, logical, iv2, int
  integer, target :: i
  real, target    :: r

  arr = [integer(2):: 99,100]
  associate (arr => [integer(2):: 1,2])
     print *, arr
     print *, [integer(2):: arr, arr]
     print *, [integer(2):: (arr, i=1,2)]
     print *, [integer(2):: (arr(i),i=1,2)]
     arr4 = [integer(2):: arr, arr]
     print *, arr4
     arr4 = [integer(2):: (arr, i=1,2)]
     print *, arr4
     arr4 = [integer(2):: (arr(i), arr(i), i=1,2)]
     print *, arr4
     arr2 = [integer(2):: arr]
     print *, arr2
     arr2 = [integer(2):: 3,4]
     print *, arr2
     associate (arr => [real:: 4.0, 5.0])
        print *, arr
     end associate
  end associate

  print *, arr ! verify that this array was untouched in the associate construct

  associate (real => [integer(2):: 1, 2.0], integer => [real(4):: 3, 4])
     associate(logical => [real:: real + [1,2]], print => [character:: 'a', 'b'])
        associate(associate => [logical:: (print(i) > 'a', i=1,2)])
           print *, real, integer, logical, print, associate
           print *, [real:: (logical(int), int=1,2)]
        end associate
     end associate
  end associate

  associate (iarr => [integer:: (iv, iv=1,2)])
     print *, iarr
  end associate

  ! Show that "iv" can be used as an associate name referring to an array, despite being declared as a scalar above
  associate (iv => [integer:: 3,4])
     print *, iv
  end associate

  ! "iv" is allowed as an ac-do-variable here, but not within the body of the associate construct
  associate (iv => [integer:: (iv, iv=5,6)])
     print *, iv
     ! here, "iv" refers to the array association introduced above
     associate (iv2 => [real:: (real(iv2), real(iv), iv2=1,2)])
        print *, iv2
     end associate
  end associate

end program acetint33
