!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* in formatted I/O
!*
!* ===================================================================

program mxminMisc28

  integer :: io_stat

  character*3, allocatable:: charArr(:,:,:)
  character*50 CHARVAR
  logical      logArr(5,6,7)

  CHARVAR = "(2A3, 35I2)"

  allocate(charArr(5,6,7))

  charArr = 'aBd'

  charArr(1:4,1:2,2:3) = 'Zui'
  charArr(4,2:3,3:5) = 'Zzz'
  charArr(4,3:,5:) = 'zTY'

  logArr = .true.
  logArr(1:3,2:5,4:) = .false.

  io_stat = -1
  write (unit=4, fmt = CHARVAR, iostat=io_stat) MAXVAL(charArr), MAX(charArr(1,1,1), charArr(2,4,1), charArr(5,6,7)), MAXLOC(charArr,dim = maxval((/-1,1,2/)), mask=logArr)

    if (io_stat .ne. 0) then
        error stop 1_4
     end if

    io_stat = -1
     close (unit=4, iostat=io_stat)
     if (io_stat .ne. 0) then
        error stop 2_4
     end if

  deallocate(charArr)

end program mxminMisc28

