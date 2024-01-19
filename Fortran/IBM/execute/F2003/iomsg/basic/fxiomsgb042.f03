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
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Close files with No Error conditions to check
!*                               if the iomsg specifier remains unchanged.
!*
!*  TEST CONDITIONS            : 1) CLOSE formatted file with direct access
!*                               2) CLOSE unfmtted file with sequential access
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb042

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

      open ( 8, access = 'DIRECT', recl = 10, err = 10, form = 'FORMATTED' )

      open ( 9, access = 'SEQUENTIAL', err = 10, form = 'UNFORMATTED' )

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = 'abc'

      close ( 8, status = 'DELETE', iostat = ios, iomsg = errmsg )

      if ( errmsg <> 'abc' ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      errmsg = 'abc'

      close ( 9, status = 'DELETE', iostat = ios, iomsg = errmsg  )

      if ( errmsg <> 'abc' ) call zzrc ( case_id )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.

