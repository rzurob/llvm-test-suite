!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-15 (original: 2006-09-27)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array
!*                               Constructor Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement derived TS
!*                               in AC same as associate name in ASSOCIATE
!*                               construct
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!*  Here we focus on features of derived types, showing that a derived type
!*  with the same name as the associate name does not cause confusion.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none
  type :: associate (kassociate_1) ! kassociate_1=4
     integer, kind :: kassociate_1
     complex(kassociate_1):: val
  end type associate

  type :: derived (lderived_1) ! lderived_1=1
     integer, len :: lderived_1
     character(lderived_1):: val
  end type derived

end module mod

program acetdt33kl

  use mod
  implicit none
  integer :: i, int
  type (associate(4)) :: a ! tcx: (4)
  type (derived(1))   :: d ! tcx: (1)

  d = derived(1)('z') ! tcx: (1)
  print *, [derived(1):: derived(1)('y')] ! tcx: (1) ! tcx: (1)
  print *, d

  a = associate(4)((1.1,2.2)) ! tcx: (4)
  print *, [associate(4):: associate(4)((1.1,2.2))] ! tcx: (4) ! tcx: (4)
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
     associate(complex => [derived(1):: (derived(1)('a'), int=1,1)]) ! tcx: (1) ! tcx: (1)
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
     associate(complex => [associate(4):: (associate(4)((1.1,2.2)), int=1,1)]) ! tcx: (4) ! tcx: (4)
        print *, complex
     end associate

  end associate

end program acetdt33kl


! Extensions to introduce derived type parameters:
! type: associate - added parameters (kassociate_1) to invoke with (4)/declare with (4) - 6 changes
! type: derived - added parameters (lderived_1) to invoke with (1)/declare with (1) - 6 changes
