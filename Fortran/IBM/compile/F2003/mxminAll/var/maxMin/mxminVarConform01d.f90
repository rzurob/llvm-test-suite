!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : diagnostic TC for MAX/MIN with variable
!*  (315313)                     as its argument
!* ===================================================================

  program mxminVarConform01d

     character*3 x(2,4)
     character*5 y(2,3)
     real*4      r4
     real*8      r8

     integer z(2,4)

     x = "ddd"
     y = "sssss"

     z = 98

      r4 = 2.3
      r8 = 9.3

     print *, max(x,x)(1,2)

     print *, max(x, y)

     print *, min(x, z)

     print *, max(x, x, y)

     print *, min(ichar("a"), char(88))

     print *, max(r4, r8)

     print *, min(x, r4, z)

  end program mxminVarConform01d

