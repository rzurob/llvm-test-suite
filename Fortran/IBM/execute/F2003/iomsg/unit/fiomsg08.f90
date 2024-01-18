! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fiomsg.presh fiomsg08
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For IOMSG= variable in I/O statements.
!*                             :
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONALITY TESTED       : IOMSG variable has assumed length.
!*                               This is a severe error.
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
          character(*) iomsg_var
          integer i
          open(11, status='new', iostat=i, iomsg=iomsg_var)
          if (len(iomsg_var) .ne. 245) error stop 1
          close(11)
          open(11, status='new', iostat=i, iomsg=iomsg_var)
          if (len(iomsg_var) .ne. 245) error stop 2
          print*, iomsg_var, '<=='
          
        END

        PROGRAM MAIN
          CHARACTER(245) iomsg_var
          CALL FMT000(iomsg_var)
        END
