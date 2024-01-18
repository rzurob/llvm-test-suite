!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: naninfio006.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : naninfio006
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : June 2nd, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE exceptions in i/o
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :testing IEEE specifications in i/o
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  use, intrinsic :: ieee_arithmetic
  
  integer(4) :: i
  real(4) :: i3e_exception
  logical :: precision_r4
 
!  call setrteopts("langlvl=95std")

  open(2,file='infinputpos.dat')
  open(3,file='infinputneg.dat')
  
  do i=1,8
    read(2,'(f20.5)') i3e_exception
!    if(.not.precision_r4(i3e_exception,0.0)) call zzrc(i)
    if(ieee_is_finite (i3e_exception)) call zzrc(i)
  end do
  
  do i=9,12
    read(3,'(f20.5)') i3e_exception
!    if(.not.precision_r4(i3e_exception,0.0)) call zzrc(i)
    if(ieee_is_finite (i3e_exception)) call zzrc(i)
  end do
  
  close(2)
  close(3)
  
end
