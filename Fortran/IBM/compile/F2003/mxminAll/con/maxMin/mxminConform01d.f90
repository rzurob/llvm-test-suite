!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : diagnostic TC for operand not comformable.
!*                               or argument of wrong type specified.
!* ===================================================================

  program mxminConform01d

     character*3 x(2,4)
     character*5 y(2,3)

     integer z(2,4)

     parameter(x = "ddd")
     parameter(y = "sssss")

     z = 98

     print *, max(x, y)

     print *, min(x, z)

     print *, max(x, x, y)

     print *, min(ichar("a"), char(88))

  end program mxminConform01d

