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
!*  NUMBER OF TESTS CONDITIONS : 4
!*
!*  DESCRIPTION                : Format specifiers D, E, F, and Q are used
!*                               without a period.
!*
!*  TEST CONDITIONS            : 1) Missing period for format code D.
!*                               2) Missing period for format code E.
!*                               3) Missing period for format code F.
!*                               4) Missing period for format code Q.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb037

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      real*4 varreal

      character*30 form

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

      form = '( D5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( E5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg

!
! TestCase 3...
!

      case_id = case_id + 1

      form = '( F5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg

!
! TestCase 4...
!

      case_id = case_id + 1

      form = '( Q5 )'

      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg

      end