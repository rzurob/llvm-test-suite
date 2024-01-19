!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
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
!*  DESCRIPTION                : The UNIT command has to be in the range 1
!*                               through 2147483647.
!*
!*  TEST CONDITIONS            : 1) I/O statements FLUSH with unit number -9.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb008

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

      FLUSH( a, err = 10, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

10    write(18,*) errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

      end                     ! End of TestCase.

