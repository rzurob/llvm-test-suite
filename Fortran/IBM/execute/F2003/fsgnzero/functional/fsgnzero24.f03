!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing f(z) = sqrt(z) = sqrt(|z|)*exp(i*tht/2)
!*                               where:
!*                                 z = x + i*y
!*                                 tht = atan2(y/x)
!*                               when
!*                                 y -> 0+ , x > 0, tht -> 0+
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

program fsgnzero24

  real tht, x, y, modz
  complex fz

  y = 1.0
  x = 1.0

  do j = 40, 1, -1
    y = y / j
    tht = atan2(y,x)
    fz = sqrt((x,y))
    print *, y, tht, fz
  end do

end