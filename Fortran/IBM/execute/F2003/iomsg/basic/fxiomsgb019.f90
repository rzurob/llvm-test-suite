!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb019.f
! %VERIFY: fort.18:fxiomsgb019.vf
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
!*  PRIMARY FUNCTIONS TESTED   : ENDFILE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1
!*                               through 2147483647,and I/O statement ENDFILE
!*                               is only for file connected for sequential
!*                               access
!*
!*  TEST CONDITIONS            : 1) I/O statements with unit number -9
!*                               2) ENDFILE statement with direct unfmtted file
!*                               3) ENDFILE statement with direct formatted file
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb019

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 a, ios

      character*300 errmsg

!
!  Unit number too small ( unit = -9 )
!

      a = -9

!
! TestCase 1...
!

      case_id = case_id + 1

      endfile ( unit = a, err = 10, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

 10   write(18,*) errmsg

      if ( ios <> 36 ) call zzrc ( case_id )

      open ( 9, access = 'DIRECT', recl = 80 )

      open ( 8, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

!
! TestCase 2...
!

      case_id = case_id + 1

      endfile ( 9, err = 20, iostat =ios, iomsg = errmsg )

      call zzrc ( case_id )

 20   write(18,*) errmsg

      if ( ios <> 17 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      endfile ( 9, err = 30, iostat =ios, iomsg = errmsg )

      call zzrc ( case_id )

 30   write(18,*) errmsg

      if ( ios <> 17 ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      end                     ! End of TestCase.
