!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : BACKSPACE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : BACKSPACE with No Error conditions to check if
!*                               the errmsg specifier remains unchanged.
!*
!*  TEST CONDITIONS            : 1) BACKSPACE sequential file with No Error cond
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb046

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 varint, ios

      character*20 form

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
!  OPEN file to read from
!

      open ( 9, access = 'SEQUENTIAL', err = 10 )

      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'

      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'

      rewind 9

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = 'abc'

      form = '( A5 )'

      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint

      backspace ( 9, iostat = ios, iomsg=errmsg )

      if ( errmsg <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 9, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.