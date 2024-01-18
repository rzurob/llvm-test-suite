!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: stop01.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : stop01
!*
!*  PROGRAMMER                 : Xing Xue
!*  DATE                       : August 10, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ERROR STOP in a CAF program
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : *_r ( thread-safe )
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : Test behaviour of the STOP statement with
!*                               a const-int-expr stop-code in a CAF program.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM stop01
      INTEGER, SAVE :: acoarr(100)[*]
      INTEGER I, SELF, SUM

      DO I=1, 100
        acoarr(I) = I
      END DO

      SELF = THIS_IMAGE()

      SYNC ALL
      if (self .eq. 1) then
        STOP 2
      else
        sum = 0
        do i=1, 100
          sum = sum + acoarr(I) 
        end do
      end if

!      SYNC ALL
      END
