! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fiomsg04.f
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
!*  FUNCTIONALITY TESTED       : IOMSG variable must not be changed
!*                               if no error occurs.
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

        SUBROUTINE FMT000
          CHARACTER*80 fmt
          CHARACTER*40 iomsg_var
          CHARACTER*40 temp
          fmt="( a )"
          temp = 'no changes to iomsg_var'
          iomsg_var = temp
          WRITE(0,"(""Test:"",I4,1x,A)") 0,fmt
          WRITE(0,fmt, iomsg=iomsg_var)
          if (iomsg_var .ne. temp) error stop 1
          if (len(iomsg_var) .ne. len(temp)) error stop 2
        END

        PROGRAM MAIN
          parameter (LC_MESSAGES = 5)
          external setlocale
          character null_str /z00/
          call setlocale(%VAL(LC_MESSAGES), nnull_str)
          CALL FMT000()
        END
