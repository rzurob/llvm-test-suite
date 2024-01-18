!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb049.f
! %VERIFY:
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
!*  PRIMARY FUNCTIONS TESTED   : WAIT
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : WAIT with No Error condition to check if the
!*                               iomsg specifier remains unchanged.
!*
!*  TEST CONDITIONS            : 1) WAIT on direct file with No Error cond.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb049

      implicit none                     ! All variables must be Declared

      integer*4 case_id, i, j           ! Test Case id under test.

      integer*4 ios, wid

      character*300 errmsg

      logical*4  there

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = 'abc'

      open ( 8, form = 'unformatted', err = 10, access = 'STREAM', asynch='yes')

      write ( 8, id = wid, err = 10 ) 'CCCCC'

      wait ( ID = wid, iostat = ios, iomsg = errmsg )

      if ( errmsg <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.

