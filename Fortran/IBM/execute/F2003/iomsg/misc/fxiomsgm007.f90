!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Declaring an iomsg variable exactly length as
!*                               the actural error message to check if the message
!*                               was truncated.
!*
!*  TEST CONDITIONS            : 1) Open new seq. formatted file as old.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgm007

      implicit none

      integer*4 case_id, ios            ! Test Case id under test.

      character*149  errmsg             ! Message 1526-006's length is 149

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      open ( 9, status = 'OLD', access = 'SEQUENTIAL', form =  &
  &    'FORMATTED', err = 100, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

100   write( 18, * )  errmsg

      if ( ios <> 6 ) call zzrc ( case_id )

! Clean up....

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
