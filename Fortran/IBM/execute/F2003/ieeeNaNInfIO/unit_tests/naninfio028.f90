!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: naninfio028.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 2nd, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE exceptions in i/o
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :testing IEEE specifications in i/o
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  use, intrinsic :: ieee_arithmetic

  integer(4) :: i
  real(8) :: i3e_exception

  call setrteopts("naninfoutput=old")

  open(2,file='nanqinputext.dat')
  open(3,file='nansinputext.dat')

  do i=1,4
    read(2,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
    write(*, '(f10.4)') i3e_exception
  end do

  do i=5,8
    read(3,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
    write(*, '(f10.4)') i3e_exception
  end do

  close(2)
  close(3)

end
