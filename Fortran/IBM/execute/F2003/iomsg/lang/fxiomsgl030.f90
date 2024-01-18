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
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : BACKSPACE is only for external file connected
!*                               for sequential access, and the UNIT number has
!*                               to be in the range 1 through 2147483647. The
!*                               BACKSPACE statement was tested in an interface.
!*
!*  TEST CONDITIONS            : 1) Backspace  with unit number -9.
!*                               2) Backspace  with direct unformatted file
!*                               3) Backspace  with direct formatted file
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      MODULE message_mod

      contains

      subroutine print_msg( unit_id,  msg_var )
         integer, intent(IN)::  unit_id
         character(LEN=*), intent(OUT)::  msg_var

         backspace ( unit = unit_id, iostat = ios, err = 30, iomsg=msg_var )

         call zzrc ( case_id )
30       write(18,*) msg_var
         if ( ios <=0 ) call zzrc ( case_id )

      end subroutine

      subroutine print_dummy1( intvar )
      integer  intvar

         write(0, *)  "dummy1"

      end subroutine print_dummy1

      subroutine print_dummy2()

         write(0, *)  "dummy2"

      end subroutine print_dummy2

      end module message_mod

      program fxiomsgl030

      use message_mod

      implicit none

      integer*4 case_id

      integer*4 ios, unit_number

      character*300 errmsg

      interface face_msg
        module procedure print_msg, print_dummy1, print_dummy2
      end interface

!
!  Initialize Return Code routine to 0
!
      case_id = 0
      call zzrc (case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      unit_number = -9

      call face_msg( unit_number, errmsg )

!
! TestCase 2...
!

      case_id = case_id + 1

      open ( 8, access = 'DIRECT', recl = 80 )

      call face_msg( 8, errmsg )

!
! TestCase 3...
!

      case_id = case_id + 1

      open ( 9, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

      call face_msg( 9, errmsg )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      end            ! End of TestCase.

