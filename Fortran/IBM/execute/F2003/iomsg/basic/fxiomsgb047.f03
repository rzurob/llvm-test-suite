!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ENDFILE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : ENDFILE with No Error condition to check if the
!*                               iomsg specifier remains unchanged.
!*
!*  TEST CONDITIONS            : 1) ENDFILE statement with sequential file
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb047

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      character*300 errmsg

      open ( 8, access = 'SEQUENTIAL', err=10 )

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = 'abc'

      endfile ( 8, iostat =ios, iomsg = errmsg )

      if ( errmsg <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                     ! End of TestCase.