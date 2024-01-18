!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb050.f
! %VERIFY:
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
!*  PRIMARY FUNCTIONS TESTED   : FLUSH
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : FLUSH with No Error condition to check if the
!*                               iomsg specifier remains unchanged
!*
!*  TEST CONDITIONS            : 1) FLUSH wiht NO Erro Conditon
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb050

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      character*300 errmsg

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = 'abc'

      OPEN ( 8, file = 'data_file', err = 20 )

      FLUSH( 8, iostat = ios, iomsg = errmsg )

      if ( errmsg <> 'abc' ) call zzrc ( case_id )

      stop ' '

20    call zzrc ( case_id + 100 )

      end                     ! End of TestCase.

