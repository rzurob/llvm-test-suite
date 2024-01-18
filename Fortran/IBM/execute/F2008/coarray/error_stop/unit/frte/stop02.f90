!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: stop02.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 10, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ERROR STOP in a CAF program
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : Test behaviour of the STOP statement without
!*                               a stop-code in a CAF program.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM stop02
      INTEGER, SAVE :: acoarr(100)[*]
      INTEGER I, SELF, SUM

      DO I=1, 100
        acoarr(I) = I
      END DO

      SELF = THIS_IMAGE()

      SYNC ALL
      if (self .eq. 1) then
        STOP
      else
        sum = 0
        do i=1, 100
          sum = sum + acoarr(I)
        end do
      end if

!      SYNC ALL

      print *, "Should not have reached this point."
      END
