!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 17, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that the intrinsics
!*                               are flagged if -qlanglvl is set to anything other than
!*                               default(extended), 2003std or 2003pure
!234567890123456789012345678901234567890123456789012345678901234567890
        INTEGER A
        LOGICAL IGNORE

        A = -1

        IGNORE = IS_IOSTAT_END(A)
        IGNORE = IS_IOSTAT_EOR(A)


      END
