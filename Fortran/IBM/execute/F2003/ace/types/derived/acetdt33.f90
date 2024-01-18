!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt33
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-09-27
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : derived TS in AC same as associate name in ASSOCIATE construct
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
!*  Here we focus on features of derived types, showing that a derived type with
!*  the same name as the associate name does not cause confusion.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  implicit none
  type :: associate
     complex:: val
  end type associate

  type :: derived
     character:: val
  end type derived

end module mod

program acetdt33

  use mod
  implicit none
  integer :: i,int
  type (associate) :: a
  type (derived)   :: d

  d = derived('z')
  print *, [derived:: derived('y')]
  print *, d

  a = associate((1.1,2.2))
  print *, [associate:: associate((1.1,2.2))]
  print *, a

  associate(associate => [logical:: (i == 1, i=1,2)])

     ! "associate" is now an array:
     associate(array => [logical:: (associate(i), i=1,1)])
        print *, array
     end associate

     ! In this context, "associate" is no longer available as a structure constructor:
     associate(complex => [ a ])
        print *, associate, complex
        print *, [real:: (merge(1.1,2.2,associate(int)), int=1,2)]
     end associate

     ! But any structure constructor not hidden by an associate name is available:
     associate(complex => [derived:: (derived('a'), int=1,1)])
        print *, complex
     end associate

  end associate

   ! turnabout is fair play:
  associate(derived => [logical:: (i == 1, i=1,2)])

     ! "derived" is now an array:
     associate(array => [logical:: (derived(i), i=1,1)])
        print *, array
     end associate

     ! In this context, "derived" is no longer available as a structure constructor:
     associate(complex => [ d ])
        print *, derived, complex
        print *, [real:: (merge(1.1,2.2,derived(int)), int=1,2)]
     end associate

     ! But any structure constructor not hidden by an associate name is available:
     associate(complex => [associate:: (associate((1.1,2.2)), int=1,1)])
        print *, complex
     end associate

  end associate

end program acetdt33
