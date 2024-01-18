!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: naninfio032.f
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
!*  TEST CASE TITLE            : naninfio032
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
  complex(16) :: i3e_exception
  real(8)  :: overlay(4)
  
  equivalence(overlay, i3e_exception)
  
  call setrteopts("naninfoutput=old")
  
  open(2,file='cmpx_nanqext.dat')
  open(3,file='cmpx_nansext.dat')
  
  do i=1,4
    read(2,'(2f30.5)') i3e_exception
    if(.not.(ieee_is_nan(overlay(1)).and.ieee_is_nan(overlay(3))) .or. &
     & (ieee_class(overlay(1)).ne.ieee_quiet_nan .or. &
     &  ieee_class(overlay(3)).ne.ieee_quiet_nan)) call zzrc(i)
    write(*, '(2f20.4)') i3e_exception
  end do
  
  do i=5,8
    read(3,'(2f30.5)') i3e_exception
    if(.not.(ieee_is_nan(overlay(1)).and.ieee_is_nan(overlay(3))) .or. &
     & (ieee_class(overlay(1)).ne.ieee_signaling_nan .or. &
     &  ieee_class(overlay(3)).ne.ieee_signaling_nan)) call zzrc(i)
    write(*, '(2f20.4)') i3e_exception
  end do
  
  close(2)
  close(3)
  
end	
