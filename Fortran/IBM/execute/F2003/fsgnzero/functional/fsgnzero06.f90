!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing theta = atan2(y/x), when
!*                               y -> 0- , x > 0
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

program fsgnzero06

  real*8 tht, x, y

  y = -1.0d0
  x = 1.0d0

  do i = 180, 1, -1
    y = y / i
    tht = atan2(y,x)
    print *, y, tht
  end do

end
