!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : WRITE FORMAT
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : The format command was used with unmatched
!*                               parentheses.
!*
!*  TEST CONDITIONS            : 1) Extra left parentheses on left side.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb034

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

      varint = 0

!
! TestCase 1...
!

      case_id = case_id + 1

      form = '( ( 50 ( I5.1 ) )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

      end                            ! End of TestCase.
