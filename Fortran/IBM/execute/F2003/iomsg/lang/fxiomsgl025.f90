!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : REWIND
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1
!*                               through 2147483647, and I/O statement REWIND
!*                               is only for file connected for sequential
!*                               access, not for direct access. The REWIND
!8                               statement was tested in a subroutine which
!*                               passes in an element of a string array in a
!*                               derived type which is a member of another
!*                               derived type, as iomsg specifier.
!*
!*  TEST CONDITIONS            : 1) I/O statements with unit number -9.
!*                               2) REWIND stmt with direct formatted file.
!*                               3) REWIND stmt with direct unformatted file.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      MODULE message_mod

        TYPE message

           integer(4)      useless
           character(300)  errmsg(5)

        END TYPE message

        TYPE message2

           integer          useless2
           TYPE ( message ) errmsg

        END TYPE message2

      contains

      subroutine print_msg( unit_id,  msg_var )
         integer, intent(IN):: unit_id
         character(LEN=*), intent(OUT)::  msg_var

         rewind ( unit_id , err = 20, iostat =ios, iomsg = msg_var )

         call zzrc ( case_id )

20       write(18,*) msg_var

         if ( ios <= 0 ) call zzrc ( case_id )

      end subroutine

      END MODULE message_mod

      program fxiomsgl025

      use message_mod

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.
      integer*4 a, ios
      character*10 varchar
      TYPE( message2 ) t_msg

!
!  Unit number too small ( unit = -9 )
!

      a = -9

!
! TestCase 1...
!

      case_id = case_id + 1

      call print_msg( a, t_msg%errmsg%errmsg(2) )

!
! Create file to rewind on ...
!
      varchar = 'trust'

      open ( 8, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

      write ( 8, err = 40, rec = 1, fmt = '( A10 )' ) varchar

      open ( 9, access = 'DIRECT', recl = 80 )

      write ( 9, err = 40, rec = 1 ) varchar

!
! TestCase 2...
!

      case_id = case_id + 1

      call print_msg( 8, t_msg%errmsg%errmsg(2) )

!
! TestCase 3...
!

      case_id = case_id + 1

      call print_msg( 9, t_msg%errmsg%errmsg(2) )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      stop ' '

 40   call zzrc ( case_id + 100 )

      end                     ! End of TestCase.
