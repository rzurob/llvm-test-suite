!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb029.f
! %VERIFY: fort.18:fxiomsgb029.vf
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN  CLOSE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The READ WRITE and PRINT statements must have
!*                               only vaild characters and vaild formats in the
!*                               format command.
!*
!*  TEST CONDITIONS            : 1) Invalid format codes for Write.
!*                               2) Invalid format codes for Read.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb029

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.
      integer*4 testout, testin

      character*4 fmt1
      character*1 fmt2
      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!
      case_id = 0
      call zzrc ( case_id )

      fmt1  = '(I2w'
      fmt2  = 'W'

      testout = 90

      open ( 9, file = 'tmpfile.vf', status = 'NEW' )

!
! TestCase 1...
!

      case_id = case_id + 1

      write ( 9, fmt = fmt1, iostat = ios, iomsg=errmsg ) testout

      write(18,*) errmsg

      rewind 9

!
! TestCase 2...
!

      case_id = case_id + 1

      read ( 9, fmt = fmt2, iostat = ios, iomsg=errmsg ) testin

      write(18,*) errmsg

! Clean up...

      close ( 9, status = 'DELETE' )

      end                           ! End of TestCase.
