!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : READ  WRITE  FORMAT
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The scale specifier is missing for the P format
!*                               code.
!*
!*  TEST CONDITIONS            : 1) Missing scale factor on write with PF fmt cd.
!*                               2) Missing scale factor on read with PF fmt cd.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb032

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      integer*4 varint

      character*10 form

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

      form = '( PF5.1 )'

      write ( 9, fmt = form , iostat = ios, iomsg = errmsg) varint

      write(18,*) errmsg

      rewind 9

!
!  TestCase 2...
!

      case_id = case_id + 1

      form = '( PF5.1 )'

      read ( 9, fmt = form , iostat = ios, iomsg = errmsg) varint

      write(18,*) errmsg

!  Clean up...

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
