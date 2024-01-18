! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fiomsg.presh fiomsg07
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONALITY TESTED       : IOMSG variable has runtime length.
!*                               This is a recoverable error.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        SUBROUTINE FMT000(i)
          CHARACTER(i) iomsg_var
          open(11, status='new', iomsg=iomsg_var)
          if (len(iomsg_var) .ne. 191) error stop 1
          read(11, iomsg=iomsg_var)
          print*, iomsg_var, '<=='
        END

        PROGRAM MAIN
          INTEGER i
          i = 191
          CALL FMT000(i)
        END
