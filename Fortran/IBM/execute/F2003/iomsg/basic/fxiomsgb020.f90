!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb020.f
! %VERIFY: fort.18:fxiomsgb020.vf
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : If STATUS is 'SCRATCH', file name must not
!*                               be specified.
!*
!*  TEST CONDITIONS            : 1) File name specified in 'SCRATCH' status.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb020

      implicit none                     ! All variables must be Declared

      integer case_id, ios               ! Test Case id under test.

      character*7 var

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!
      case_id   = 0
      call zzrc ( 0 )

!
! TestCase 1...
!

      case_id = case_id + 1          ! Increment to new TestCase id...

      var = 'SCRATCH'

      open ( 9, file = 'file1', status = var, iostat = ios, iomsg=errmsg )

      write(18,*) errmsg

      if ( ios .eq. 0 ) call zzrc ( case_id )

! Clean up...

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
