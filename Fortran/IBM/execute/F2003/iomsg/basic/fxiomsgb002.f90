!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb002.f
! %VERIFY: fort.18:fxiomsgb002.vf
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
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : Internal and external files are created, then
!*                               WRITE and READ are performed.
!*
!*  TEST CONDITIONS            : 1) Read past end of record seq.unformatted ext.
!*                               2) Read past end of record dir. formatted ext.
!*                               3) Read past end of record seq. formatted int.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb002

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      character*10 varchar

      character*300 errmsg

      character*6 unit9 ( 1 )

      call setrteopts("cnverr=no")

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! Create files to read from.
!

      open ( 10, access = 'SEQUENTIAL', form = 'UNFORMATTED',err = 100 )

      write ( 10, err = 100 ) '\FxXxXxX'

      open ( 11, access = 'DIRECT', form = 'FORMATTED', err = 100, recl = 6 )

      write ( 11, fmt = '( A )', rec = 1, err = 100 ) 'xXxXxX'

      write ( unit9, fmt = '( A )', err = 100 ) 'xXxXxX'

      rewind 10

!
! TestCase 1...
!

      case_id = case_id + 1

      read ( 10, iostat = ios, iomsg = errmsg ) varchar

      write(18,*) errmsg

      if ( ios <> 0 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      read ( 11, fmt = '( A )', rec = 1, iostat = ios, iomsg = errmsg ) varchar

      write(18,*) errmsg

      if ( ios <> 0 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      read ( unit9, fmt = '( A11 )', iostat = ios, iomsg = errmsg ) varchar

      write(18,*) errmsg

      if ( ios <> 0 ) call zzrc ( case_id )

      close ( 10, status = 'DELETE' )

      close ( 11, status = 'DELETE' )

      stop ' '

100   call zzrc ( case_id + 100 )

      end                            ! End of TestCase.

