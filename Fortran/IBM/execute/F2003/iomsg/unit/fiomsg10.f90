! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fiomsg.presh fiomsg10
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
!*  FUNCTIONALITY TESTED       : IOMSG variable in Module.
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

        Module mod1
          character(191), protected :: iomsg_var
          contains
            subroutine sub()
              open(11, status='new')
              read(11, iomsg=iomsg_var)
            end subroutine sub
        end module 
        
        PROGRAM MAIN
          use mod1
          call sub()
          print*, iomsg_var, '<=='
        END
