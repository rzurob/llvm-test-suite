!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : WRITE  FORMAT
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Negative reptition counter in FORMAT is not
!*                               allowed in I/O statements.
!*
!*  TEST CONDITIONS            : 1) Negative repetition counter for I format cd.
!*                               2) Negative repetition counter for X format cd.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb031

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      integer*4 varint/100/

      character*20 form

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

      form = '( -50  ( I5 ) )'

      write(0, fmt = form, iostat = ios, iomsg=errmsg ) varint

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( -50 ( 5X ) )'

      write(0, fmt = form, iostat = ios, iomsg=errmsg ) varint

      write(18,*) errmsg

      end                            ! End of TestCase.