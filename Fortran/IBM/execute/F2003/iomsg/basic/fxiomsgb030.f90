!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb030.f
! %VERIFY: fort.18:fxiomsgb030.vf
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
!*  PRIMARY FUNCTIONS TESTED   : FORMAT WRITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Some FORMAT specifiers do not allow a
!*                               repetition counter in I/O statements. TL
!*                               and BN are tested in this test case with
!*                               WRITE statement.
!*
!*  TEST CONDITIONS            : 1) Repetition specified for format code TL.
!*                               2) Repetition specified for format code BN.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb030

      implicit none

      integer*4 case_id, ios

      integer*4 varint

      character*30 form

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

      form = '( 5 TL5 )'

      write(0, fmt = form, iostat = ios, iomsg=errmsg) varint

      write(18, *) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( 5 BN )'

      write(0, fmt = form, iostat = ios, iomsg=errmsg) varint

      write(18, *) errmsg

      end                            ! End of TestCase.
