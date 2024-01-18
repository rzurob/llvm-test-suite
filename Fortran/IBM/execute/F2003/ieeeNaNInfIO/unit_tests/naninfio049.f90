!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: naninfio0049.f
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
!*  TEST CASE TITLE            : naninfio0049
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
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  DESCRIPTION                :testing IEEE specifications in i/o
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  use, intrinsic :: ieee_arithmetic
  
  integer(4) :: i
  complex(4) :: i3e_exception
  
  call setrteopts("naninfoutput=old")
  
  open(2,file='cmpx_nanqext.dat')
  open(3,file='cmpx_nansext.dat')
  open(12,file='cmpx_nanq2003std.dat')
  open(13, file='cmpx_nans2003std.dat')
  open(22,file='cmpx_infpos.dat')
  open(23,file='cmpx_infneg.dat')
  
  !read/write nanq
  do i=1,4
    read(2,'(2f30.5)') i3e_exception
    if(.not.(ieee_is_nan(real(i3e_exception)).and.ieee_is_nan(aimag(i3e_exception))) .or. &
     & (ieee_class(real(i3e_exception)).ne.ieee_quiet_nan .or. &
     &  ieee_class(aimag(i3e_exception)).ne.ieee_quiet_nan)) call zzrc(i)
     write(*,'(2f20.4)') i3e_exception
  end do
  !read/write nans
  do i=5,8
    read(3,'(2f30.5)') i3e_exception
    if(.not.(ieee_is_nan(real(i3e_exception)).and.ieee_is_nan(aimag(i3e_exception))) .or. &
     & (ieee_class(real(i3e_exception)).ne.ieee_signaling_nan .or. &
     &  ieee_class(aimag(i3e_exception)).ne.ieee_signaling_nan)) call zzrc(i)
     write(*,'(2f20.4)') i3e_exception
  end do
  
  !read/write nan(q)
  do i=9,15
    read(12,'(2f30.5)') i3e_exception
    if(.not.(ieee_is_nan(real(i3e_exception)).and.ieee_is_nan(aimag(i3e_exception))) .or. &
     & (ieee_class(real(i3e_exception)).ne.ieee_quiet_nan .or. &
     &  ieee_class(aimag(i3e_exception)).ne.ieee_quiet_nan)) call zzrc(i)
     write(*,'(2f20.4)') i3e_exception
  end do
  
  !read/write nan(s)
  do i=16,18
    read(13,'(2f30.5)') i3e_exception
    if(.not.(ieee_is_nan(real(i3e_exception)).and.ieee_is_nan(aimag(i3e_exception))) .or. &
     & (ieee_class(real(i3e_exception)).ne.ieee_signaling_nan .or. &
     &  ieee_class(aimag(i3e_exception)).ne.ieee_signaling_nan)) call zzrc(i)
    write(*,'(2f20.4)') i3e_exception
  end do
  
  !read/write +inf
  do i=19,26
    read(22,'(2f30.5)') i3e_exception
     if((ieee_is_finite(real(i3e_exception)).or.ieee_is_finite(aimag(i3e_exception)) ).or. &
     &(ieee_class(real(i3e_exception)).ne.ieee_positive_inf .or. &
     &ieee_class(aimag(i3e_exception)).ne.ieee_positive_inf)) call zzrc(i)
     write(*,'(2f20.4)') i3e_exception
  end do
  
  !read/write -inf
  do i=26,29
    read(23,'(2f30.5)') i3e_exception
     if((ieee_is_finite(real(i3e_exception)).or.ieee_is_finite(aimag(i3e_exception)) ).or. &
     &(ieee_class(real(i3e_exception)).ne.ieee_negative_inf .or. &
     &ieee_class(aimag(i3e_exception)).ne.ieee_negative_inf)) call zzrc(i)
     write(*,'(2f20.4)') i3e_exception
  end do
  
  close(2)
  close(3)
  close(12)
  close(13)
  close(22)
  close(23)
  
end		
