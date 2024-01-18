!******************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb001.f
! %VERIFY: fort.18:fxiomsgb001.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!******************************************************************************

!*  ===========================================================================
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
!*  DESCRIPTION                : Internal and external files are created, WRITE,
!*                               REWIND then a READ is preformed.
!*
!*  TEST CONDITIONS            : 1) Read past end of file seq. unformatted ext.
!*                               2) Read past end of file Namelist external.
!*                               3) Read past end of file seq. formatted int.
!* ============================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!******************************************************************************

      program fxiomsgb001

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 varint/1/, ios

      character*10 varchar

      character*300 errmsg

      character*100 unit9 ( 1 )

      namelist /naml1/ varint

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! Create files to read from.
!

      open ( 10, access = 'SEQUENTIAL', form = 'UNFORMATTED', err = 100 )

      write ( 10, err = 100 ) 'xXxXxXxXxX'

      write ( 12, fmt = naml1, err = 100)

      write ( unit9, fmt = '( A6 )', err = 100 ) 'xXxXxXxXxX'

      rewind 10
      rewind 12

!
! TestCase 1...
!

      case_id = case_id + 1

      read ( 10, err = 20, iostat = ios ) varchar
      read ( 10, err = 20, iostat = ios,  iomsg = errmsg ) varchar

20    write( 18 , * ) errmsg
      if ( ios .eq. 0 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id +1

      read (12, fmt = naml1 , err = 100)
      read ( 12, fmt = naml1,  iostat = ios,  iomsg = errmsg )

40    write( 18 , * ) errmsg
      if ( ios .eq. 0 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      read ( unit9, fmt = '(A10)', iostat = ios, iomsg = errmsg ) &
   &  varchar,varchar

50    write( 18 , * ) errmsg
      if ( ios .eq. 0 ) call zzrc ( case_id )

      close ( 10, status = 'DELETE' )

      close ( 12, status = 'DELETE' )

      stop ' '

100   call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
