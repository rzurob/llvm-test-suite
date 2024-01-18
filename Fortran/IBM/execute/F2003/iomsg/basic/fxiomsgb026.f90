!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb026.f
! %VERIFY: fort.18:fxiomsgb026.vf
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
!*  PRIMARY FUNCTIONS TESTED   : WRITE  FORMAT
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The exponent format codes are used with an
!*                               invalid exponent.
!*
!*  TEST CONDITIONS            : 1) Invalid exponent specifier for E E format cd
!*                               2) Invalid exponent specifier for G D format cd
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb026

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      real*4 varreal

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

      form = '( E5.2E2004 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( G5.2D2004 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg

      end                ! End of TestCase.
