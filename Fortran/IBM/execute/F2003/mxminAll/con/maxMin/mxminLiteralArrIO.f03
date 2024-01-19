!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal in I/O
!*
!* ===================================================================

  program mxminLiteralArrIO

     integer io_stat

     character*50 CHARVAR

     CHARVAR = "(6A8, 6A8, 6A8)"

     io_stat = -1
     write (unit=9, fmt = CHARVAR, iostat=io_stat) max(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"bca","bca","bca","bca","bca","bca"/), (/2,3/))), min(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"bca","bca","bca","bca","bca","bca"/), (/2,3/))) , max(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"bca","bca","bca","bca","bca","bca"/), (/2,3/)), "zzzzaaaa")

     if (io_stat .ne. 0) then
        error stop 1_4
     end if

     io_stat = -1
     close (unit=9, iostat=io_stat)
     if (io_stat .ne. 0) then
        error stop 2_4
     end if

  end program mxminLiteralArrIO

