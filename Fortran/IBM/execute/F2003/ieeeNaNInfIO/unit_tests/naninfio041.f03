!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 2nd, 2006
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
  complex(4) :: i3e_exception

  call setrteopts("naninfoutput=old")

  open(2,file='cmpx_nanqext.dat')
  open(3,file='cmpx_nansext.dat')

  do i=1,4
    read(2,'(2en30.5)') i3e_exception
    if(.not.(ieee_is_nan(real(i3e_exception)).and.ieee_is_nan(aimag(i3e_exception))) .or. &
     & (ieee_class(real(i3e_exception)).ne.ieee_quiet_nan .or. &
     &  ieee_class(aimag(i3e_exception)).ne.ieee_quiet_nan)) call zzrc(i)
    write(*, '(2en20.4)') i3e_exception
  end do

  do i=5,8
    read(3,'(2en30.5)') i3e_exception
    if(.not.(ieee_is_nan(real(i3e_exception)).and.ieee_is_nan(aimag(i3e_exception))) .or. &
     & (ieee_class(real(i3e_exception)).ne.ieee_signaling_nan .or. &
     &  ieee_class(aimag(i3e_exception)).ne.ieee_signaling_nan)) call zzrc(i)
    write(*, '(2en20.4)') i3e_exception
  end do

  close(2)
  close(3)

end
