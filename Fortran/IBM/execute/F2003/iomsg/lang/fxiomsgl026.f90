!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl026.f
! %VERIFY: fort.18:fxiomsgl026.vf
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : An error is given when an OPEN statement tries
!*                               to open a new file with status old. The OPEN
!*                               statement was tested in a subroutine which
!*                               passes in a string member of an element of a
!*                               derived type array, as iosmsg specifier.
!*
!*  TEST CONDITIONS            : 1) Open new seq. formatted file as old.
!*                               2) Open new direct unformatted file as old.
!*
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

      contains

      subroutine print_msg( unit_id,  access_var, form_var, msg_var )
         integer, intent(IN):: unit_id
         character(LEN=*), intent(IN)::   access_var
         character(LEN=*), intent(IN)::   form_var
         character(LEN=*), intent(OUT)::  msg_var

        open ( unit_id, status = 'OLD', access = access_var, form =  &
  &     form_var, err = 100, iostat = ios, iomsg = msg_var )

        call zzrc ( case_id )

100     write( 18, * ) msg_var

        if ( ios <> 6 ) call zzrc ( case_id )

      end subroutine

      END MODULE message_mod

      program fxiomsgl026

      use message_mod

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      TYPE ( message )  t_msg(3)

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      call print_msg( 9, 'SEQUENTIAL', 'FORMATTED', t_msg(2)%errmsg(4) )

!
! TestCase 2...
!

      case_id = case_id + 1

      call print_msg( 9, 'STREAM', 'UNFORMATTED', t_msg(2)%errmsg(4) )

! Clean up....

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
