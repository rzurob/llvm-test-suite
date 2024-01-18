! GM DTP extension using:
! ftcx_dtp -qnok -qnol /tstdev/F2003/ace/diag/types/derived/acetdt33d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt33d
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt33d
!*                               by David Forster)
!*  DATE                       : 2007-11-28 (original: 2006-09-27)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!*  Similarly named tests exist in the intrinsic, derived, and "none" types
!*  sub-buckets, as well as in their diagnostic counterparts.
!*
!*  Here we focus on features of derived types, showing that attempting to use
!*  a structure constructor shadowed by an associate name will be flagged as an
!*  error.
!*
!*  Most importantly, however, type specifiers in ACs are not confused by
!*  associate names identical to intrinsic or derived types.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none
  type :: associate(k1)    ! (4)
     integer, kind :: k1
     complex(k1)   :: val
  end type associate

  type :: derived(l1)    ! (1)
     integer, len  :: l1
     character(l1) :: val
  end type derived

end module mod

program acetdt33dext

  use mod
  implicit none
  integer :: i

  ! Alone, this is okay
  print *, [associate(4):: associate(4)((1.1,2.2))]

  associate(associate => [logical:: (i==1, i=1,2)], derived => [integer:: (i, i=1,1)] )

     ! "associate" is now an array, so this is okay:
     associate(array => [logical:: (associate(i), i=1,1)])
        print *, array
     end associate

     ! In this context, "associate" is no longer available as a structure constructor, so this is bad:
     associate(complex => [associate(4):: associate(4)((1.1,2.2))])
        print *, associate, complex
     end associate

     ! ditto for "derived":
     associate(bland => [derived(1):: derived(1)('a')])
        print *, derived, bland
     end associate

  end associate

end program acetdt33dext
