!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl024.f
! %VERIFY: fort.18:fxiomsgl024.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f message_mod.mod
! %END
!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ENDFILE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1
!*                               through 2147483647, and I/O statement ENDFILE
!*                               is only for file connected for sequential
!*                               access. The ENDFILE statement was tested in
!*                               a subroutine which passes in a string which
!*                               is a member of a derived type, as the iomsg
!*                               specifier.
!*
!*  TEST CONDITIONS            : 1) I/O statements with unit number -9.
!*                               2) ENDFILE stmt with direct unformatted file.
!*                               3) ENDFILE stmt with direct formatted file.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      MODULE message_mod

        TYPE message

           integer(4)      useless
           character(300)  errmsg

        END TYPE message

      contains

      subroutine print_msg( unit_id,  msg_var )
         integer, intent(IN):: unit_id
         character(LEN=*), intent(OUT)::  msg_var

         endfile ( unit_id, err = 20, iostat =ios, iomsg = msg_var )

         call zzrc ( case_id )

 20      write(18,*) msg_var

         if ( ios <> 17 ) call zzrc ( case_id )

      end subroutine

      END MODULE message_mod

      program fxiomsgl024

      use message_mod

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.
      integer*4 a, ios
      TYPE(message) t_msg

!
!  Unit number too small ( unit = -9 )
!

      a = -9

!
! TestCase 1...
!

      case_id = case_id + 1

      call print_msg( a, t_msg.errmsg )

      open ( 9, access = 'DIRECT', recl = 80 )

      open ( 8, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

!
! TestCase 2...
!

      case_id = case_id + 1

      call print_msg( 9, t_msg.errmsg )

!
! TestCase 3...
!

      case_id = case_id + 1

      call print_msg( 9, t_msg.errmsg )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      end                     ! End of TestCase.
