!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb040.f
! %VERIFY: fort.18:fxiomsgb040.vf
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
!*  PRIMARY FUNCTIONS TESTED   : FORMAT  WRITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
!*
!*  DESCRIPTION                : The Z, O, B, and D format codes are used with
!*                               print specifier that is too large.
!*
!*  TEST CONDITIONS            : 1) Invalid print specifier for D format code.
!*                               2) Invalid print specifier for Z format code.
!*                               3) Invalid print specifier for O format code.
!*                               4) Invalid print specifier for B format code.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb040

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      integer*4 varint

      character*20 form

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

      form = '( D5.2001 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( Z5.2001 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

!
! TestCase 3...
!

      case_id = case_id + 1

      form = '( O5.2001 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

!
! TestCase 4...
!

      case_id = case_id + 1

      form = '( B5.2001 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

      end                ! End of TestCase.
