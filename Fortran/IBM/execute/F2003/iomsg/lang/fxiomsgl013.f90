!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl013.f
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
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : INQUIRE with No Error condition to check if the
!*                               iomsg specifier remains unchanged. The iomsg
!*                               specifier in INQUIRE statement was passed in
!*                               by one element of a string array.
!*
!*  TEST CONDITIONS            : 1) INQUIRE on direct file with No Error cond.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl013

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.
      integer*4 ios
      character*300 errmsg (5)
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

      errmsg(2) = 'abc'

      open ( 8, file='trust', err = 10, access = 'DIRECT', recl = 2 )

      inquire ( 8, exist=there, iostat = ios, iomsg = errmsg(2) )

      if ( errmsg(2) <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.

