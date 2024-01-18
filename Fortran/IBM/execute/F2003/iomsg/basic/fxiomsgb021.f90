!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb021.f
! %VERIFY: fort.18:fxiomsgb021.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : CLOSE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : The CLOSE statement is specified KEEP for a
!*                               SCRATCH file.  This is invalid.
!*
!*  TEST CONDITIONS            : 1) OPEN SCRATCH file and CLOSE as KEEP.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb021

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!
      case_id = 0
      call zzrc ( case_id )

!
!  TestCase 1...
!

      case_id = case_id + 1

      open ( 9, status = 'SCRATCH', err = 100 )

      close ( 9, status = 'KEEP', iostat = ios, iomsg=errmsg )

      write(18,*) errmsg

      if ( ios .eq. 0 ) call zzrc ( case_id )

! Clean up...

      open ( 9, file = 'fort.9' )

      close ( 9, status = 'DELETE' )

      stop ' '

100   call zzrc ( case_id )

      end                            ! End of TestCase.
