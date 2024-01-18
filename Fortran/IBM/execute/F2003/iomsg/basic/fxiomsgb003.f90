!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb003.f
! %VERIFY: fort.18:fxiomsgb003.vf
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
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : An error is given when an OPEN statement tries
!*                               to open a new file with status OLD.
!*
!*  TEST CONDITIONS            : 1) Open new seq. formatted file as old.
!*                               2) Open new direct unformatted file as old.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb003

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
! TestCase 1...
!

      case_id = case_id + 1

      open ( 9, status = 'OLD', access = 'SEQUENTIAL', form =  &
  &    'FORMATTED', err = 100, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

100   write( 18, * ) errmsg
      if ( ios <> 6 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      open ( 9, status = 'OLD', access = 'DIRECT', form = &
   &   'UNFORMATTED', err = 400, iostat = ios, recl = 20 )

      call zzrc ( case_id )

400   write( 18, * ) errmsg
      if ( ios <> 6 ) call zzrc ( case_id )

! Clean up....

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
