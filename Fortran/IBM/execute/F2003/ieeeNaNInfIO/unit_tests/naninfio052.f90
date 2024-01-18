!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: naninfio0052.f
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
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  DESCRIPTION                :testing IEEE specifications in i/o
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  use, intrinsic :: ieee_arithmetic

  integer(4) :: i
  real(4) :: i3e_exception

  call setrteopts("naninfoutput=default")

  open(2,file='nanqinputext.dat')
  open(3,file='nansinputext.dat')
  open(12,file='nanqinput2003std.dat')
  open(13, file='nansinput2003std.dat')
  open(22,file='infinputpos.dat')
  open(23,file='infinputneg.dat')

  !read/write nanq
  do i=1,4
    read(2,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do
  !read/write nans
  do i=5,8
    read(3,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write nan(q)
  do i=9,15
    read(12,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write nan(s)
  do i=16,18
    read(13,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
    &  ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
    write(*,'(f10.4)') i3e_exception
  end do

  !read/write +inf
  do i=19,26
    read(22,'(f20.5)') i3e_exception
     if(ieee_is_finite(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_positive_inf) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write -inf
  do i=26,29
    read(23,'(f20.5)') i3e_exception
     if(ieee_is_finite(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_negative_inf) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  rewind(2)
  rewind(3)
  rewind(12)
  rewind(13)
  rewind(22)
  rewind(23)

  call setrteopts("naninfoutput=2003std")

    !read/write nanq
  do i=30,33
    read(2,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do
  !read/write nans
  do i=34,37
    read(3,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write nan(q)
  do i=38,44
    read(12,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write nan(s)
  do i=45,47
    read(13,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
    &  ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
    write(*,'(f10.4)') i3e_exception
  end do

  !read/write +inf
  do i=48,55
    read(22,'(f20.5)') i3e_exception
     if(ieee_is_finite(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_positive_inf) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write -inf
  do i=55,58
    read(23,'(f20.5)') i3e_exception
     if(ieee_is_finite(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_negative_inf) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  rewind(2)
  rewind(3)
  rewind(12)
  rewind(13)
  rewind(22)
  rewind(23)

  call setrteopts("naninfoutput=old")

    do i=59,62
    read(2,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do
  !read/write nans
  do i=63,66
    read(3,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write nan(q)
  do i=67,73
    read(12,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_quiet_nan) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write nan(s)
  do i=74,76
    read(13,'(f20.5)') i3e_exception
    if(.not.ieee_is_nan(i3e_exception) .or. &
    &  ieee_class(i3e_exception).ne.ieee_signaling_nan) call zzrc(i)
    write(*,'(f10.4)') i3e_exception
  end do

  !read/write +inf
  do i=77,84
    read(22,'(f20.5)') i3e_exception
     if(ieee_is_finite(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_positive_inf) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  !read/write -inf
  do i=85,88
    read(23,'(f20.5)') i3e_exception
     if(ieee_is_finite(i3e_exception) .or. &
     & ieee_class(i3e_exception).ne.ieee_negative_inf) call zzrc(i)
     write(*,'(f10.4)') i3e_exception
  end do

  close(2)
  close(3)
  close(12)
  close(13)
  close(22)
  close(23)

end
