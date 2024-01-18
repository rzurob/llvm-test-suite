!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/chkmsg.sh siostat196
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : POS= on non-stream
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat, i
       i = 10
       open(unit=11, access='direct', recl=10)
       read(11, pos=3, iostat=iostat) i
       if (iostat /= 196) error stop 1

       read(11, pos=3) i
       close(11)

       open(unit=11, access='sequential', form='unformatted')
       read(11, pos=3, iostat=iostat) i
       if (iostat /= 196) error stop 2

       read(11, pos=3) i
       close(11)

       end
