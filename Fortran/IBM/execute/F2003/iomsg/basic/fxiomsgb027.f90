!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb027.f
! %VERIFY: fort.18:fxiomsgb027.vf
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
!*  PRIMARY FUNCTIONS TESTED   : READ
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : This test case READs data from a file that is
!*                               invalid for Z or B format codes.
!*
!*  TEST CONDITIONS            : 1) READ invalid data with B format code, direct
!*                               2) READ invalid data with Z format code,sequent
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb027

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios

      integer*4 int

      character*10 varchar

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! Define variables with dummy constants and make READ test file.
!

      varchar       = 'Gg3f'

!
! Create file to be read from
!

      open ( 9, access = 'DIRECT', recl = 10, err = 10, form = 'FORMATTED' )

      write ( 9, err = 10, rec = 1, fmt = '( A4 )' ) varchar

      open ( 8, access = 'SEQUENTIAL', err = 10, form = 'FORMATTED' )

      write ( 8, err = 10, fmt = '( A4 )' ) varchar

      rewind 8

!
! TestCase 1...
!

      case_id = case_id + 1

      read ( 9, fmt = '( B4 )', rec = 1, iostat = ios, iomsg=errmsg ) int

      write(18, *) errmsg

      if ( ios <> 56 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      read ( 8, fmt = '( Z4 )', iostat = ios, iomsg=errmsg ) int

      write(18, *) errmsg

      if ( ios <> 56 ) call zzrc ( case_id )

! Clean up...

      close ( 9, status = 'DELETE' )

      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
