!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : diagnostic TC for operand not comformable.
!*                               or argument of wrong type specified with
!*                               argument using literal.
!* ===================================================================

  program mxminLiteralConform01d

     integer z

     z = 98

     print *, max((/"aa", "bb"/), (/"aa", "bb", "cc"/))

     print *, min("aa", z)

     print *, max((/"aa", "bb"/), (/"aa", "bb"/), (/"aa", "bb", "cc"/))

  end program mxminLiteralConform01d

