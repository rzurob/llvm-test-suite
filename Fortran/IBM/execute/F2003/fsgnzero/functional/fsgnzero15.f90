!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 05/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing f(z) = log(z) = log|z| + i*theta
!*                               where: 
!*                                 z = x + i*y
!*                                 theta = atan2(y/x)
!*                               when
!*                                 y -> 0- , x < 0, theta -> -pi
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fsgnzero15

  real tht, x, y, modz
  complex fz

  y = -1.0
  x = -1.0

  do j = 40, 1, -1
    y = y / j
    tht = atan2(y,x)
    fz = log((x,y))
    print *, y, tht, fz
  end do

end
