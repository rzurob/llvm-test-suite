!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb006.f
! %VERIFY: fort.18:fxiomsgb006.vf
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
!*  PRIMARY FUNCTIONS TESTED   : BACKSPACE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : BACKSPACE is only for external file connected
!*                               for sequential access, and the UNIT number has
!*                               to be in the range 1 through 2147483647.
!*
!*  TEST CONDITIONS            : 1) Backspace  with unit number -9.
!*                               2) Backspace  with direct unformatted file
!*                               3) Backspace  with direct formatted file
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb006

      implicit none

      integer*4 case_id

      integer*4 ios, unit_number

      character*300 errmsg

!
!  Initialize Return Code routine to 0
!
      case_id = 0
      call zzrc (case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      unit_number = -9

      backspace ( unit = unit_number, iostat = ios, iomsg=errmsg, err = 10 )

      call zzrc ( case_id )
 10   write( 18, * ) errmsg
      if ( ios <= 0 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      open ( 8, access = 'DIRECT', recl = 80 )

      backspace ( unit = 8, iostat = ios, err = 30, iomsg=errmsg )

      call zzrc ( case_id )
 30   write(18, *) errmsg
      if ( ios <=0 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      open ( 9, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

      backspace ( unit = 9, iostat = ios, err = 40 ,iomsg=errmsg)

      call zzrc ( case_id )
 40   write(18, *) errmsg
      if ( ios <=0 ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      end            ! End of TestCase.

