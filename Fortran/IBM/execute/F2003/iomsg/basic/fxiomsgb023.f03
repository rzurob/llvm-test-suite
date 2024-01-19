!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : CLOSE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Unit number can't be 0.
!*
!*  TEST CONDITIONS            : 1) I/O statements with unit number 0.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb023

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      integer*4 a

      character*300 errmsg

      a = 0

!
! TestCase 1...
!

      case_id = case_id + 1

      close ( unit = a, err = 10, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

10    write(18,*) errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

      end                      ! End of TestCases
