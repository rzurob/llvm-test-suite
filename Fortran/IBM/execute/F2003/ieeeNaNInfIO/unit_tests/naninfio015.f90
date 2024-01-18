!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 2nd, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE exceptions in i/o
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldinfnan
!*
!*  DESCRIPTION                :testing IEEE specifications in i/o
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  use, intrinsic :: ieee_arithmetic

  integer(4) :: i
  real(4) :: i3e_exception, num1, num2, num3, num4

  call setrteopts("naninfoutput=2003std")

  num1=0.0
  num2=0.0
  num3=1.0
  num4=0.0000000000000000000000000000000000000001

  !+infinity
  i3e_exception=z'7F800000'
  write(*,'(f10.4)') i3e_exception

  !-infinity
  i3e_exception=z'FF800000'
  write(*,'(f10.4)') i3e_exception

  !nanq
  i3e_exception=z'7fffc000'
  write(*,'(f10.4)') i3e_exception

  !nanq
  i3e_exception=z'ffffc000'
  write(*,'(f10.4)') i3e_exception

  !nans
  i3e_exception=z'FF800001'
  write(*,'(f10.4)') i3e_exception

  !nans
  i3e_exception=z'7F800001'
  write(*,'(f10.4)') i3e_exception

  !+infinity
  i3e_exception=num3/num4
  write(*,'(f10.4)') i3e_exception

  !-infinity
  i3e_exception=-num3/num4
  write(*,'(f10.4)') i3e_exception

  !nanq
  i3e_exception=num1/num2
  write(*,'(f10.4)') i3e_exception

  !nanq
  i3e_exception= -num1/num2
  write(*,'(f10.4)') i3e_exception

end


