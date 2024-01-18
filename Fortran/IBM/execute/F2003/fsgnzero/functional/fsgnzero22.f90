!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing f(z) = log(z) = log|z| + i*theta
!*                               where:
!*                                 z = x + i*y
!*                                 theta = atan2(y/x)
!*                               when
!*                                 y -> 0- , x > 0, theta -> 0+
!*                                 (real*16 has the same behaviour as before)
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

program fsgnzero22

  real*16 tht, x, y, modz
  complex*32 fz

  y = -1.0q0
  x = 1.0q0

  do j = 180, 1, -1
    y = y / j
    tht = atan2(y,x)
    fz = log((x,y))
    print *, y, tht, fz
  end do

end
