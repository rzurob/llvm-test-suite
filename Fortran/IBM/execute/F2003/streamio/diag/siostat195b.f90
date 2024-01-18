!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/chkmsg.sh siostat195b
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
!*  DESCRIPTION                : Invalid POS= value
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat, z
       z = -1
       open(unit=11, access='stream', status='scratch')
       write (11, pos=z, iostat=iostat) 10
       if (iostat /= 195) error stop 1

       z = -20
       write (11, pos=z) 10
       end
