!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb036.f
! %VERIFY: fort.18:fxiomsgb036.vf
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
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : T, TL, and TR format specifiers were given
!*                               invalid position specifiers.
!*
!*  TEST CONDITIONS            : 1) Invalid position specifier for T fmt code.
!*                               2) Invalid position specifier for TL fmt code.
!*                               3) Invlaid position specifier for TR fmt code.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb036

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

      form = '( T-5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( TL-5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

!
! TestCase 3...
!

      case_id = case_id + 1

      form = '( TR-5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

      end                            ! End of TestCase.
