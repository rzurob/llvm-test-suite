! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fiomsg05.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For IOMSG= variable in I/O statements.
!*                             :
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONALITY TESTED       : IOMSG variable is passed as dummy
!*                               argument with assumed length. It must
!*                               not be changed if no error occurs.
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        SUBROUTINE FMT000(iomsg_var)
          CHARACTER*80 fmt
          CHARACTER*(*) iomsg_var
          fmt="( a )"
          WRITE(0,"(""Test:"",I4,1x,A)") 0,fmt
          WRITE(0,fmt, iomsg=iomsg_var)
        END

        PROGRAM MAIN
          CHARACTER*40 iomsg_var
          CHARACTER*40 temp
          parameter (LC_MESSAGES = 5)
          external setlocale
          character null_str /z00/
          call setlocale(%VAL(LC_MESSAGES), nnull_str)
          temp = 'no changes to iomsg_var'
          iomsg_var = temp
          CALL FMT000(iomsg_var, temp)
          if (iomsg_var .ne. temp) error stop 1
          if (len(iomsg_var) .ne. len(temp)) error stop 2
        END
