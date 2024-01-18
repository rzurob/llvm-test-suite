! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fiomsg06.f
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
!*  FUNCTIONALITY TESTED       : IOMSG variable has runtime length. It
!*                               must not be changed if no error occurs.
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
          CHARACTER*80 fmt
          CHARACTER(i) iomsg_var
          fmt="( a )"
          WRITE(0,"(""Test:"",I4,1x,A)") 0,fmt
          WRITE(0,fmt, iomsg=iomsg_var)
          if (len(iomsg_var) .ne. 7) error stop 1
        END

        PROGRAM MAIN
          INTEGER i
          parameter (LC_MESSAGES = 5)
          external setlocale
          character null_str /z00/
          call setlocale(%VAL(LC_MESSAGES), nnull_str)
          i = 7
          CALL FMT000(i)
        END
