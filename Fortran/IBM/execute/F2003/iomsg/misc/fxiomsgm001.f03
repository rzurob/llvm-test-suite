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
!*  DESCRIPTION                : When an I/O error happened, the corresponding
!*                               error message with be put in iomsg specifier.
!*                               IF the size of iomsg specifier is bigger than
!*                               the actual error message, then the rest of it
!*                               should be filled with all blanks.
!*
!*  TEST CONDITIONS            : 1) Open new seq. formatted file as old.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgm001

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios, msg_len

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

      open ( 9, status = 'OLD', access = 'SEQUENTIAL', form =  &
  &    'FORMATTED', err = 100, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

100   msg_len = LEN( TRIM( errmsg ) )

      errmsg ( msg_len + 1 : 300 ) = ' '

      write( 18, * ) errmsg

      if ( ios <> 6 ) call zzrc ( case_id )

! Clean up....

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
