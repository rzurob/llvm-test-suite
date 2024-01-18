!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl023.f
! %VERIFY: fort.18:fxiomsgl023.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : READ
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Integer values that are larger than the allowed
!*                               are read from direct external files. The READ
!*                               statement was tested in a subroutine which
!*                               passes in an element of a string array as iomsg
!*                               specifier.
!*
!*  TEST CONDITIONS            : 1) Max integer*1 with external file.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl023

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      integer*1 varint1

      character*20, form

      character*300 errmsg(5)

!
! Initialize Return Code routine to SUCCESS...
!
      case_id = 0
      call zzrc ( case_id )

!
! Open files to read from.
!

      open ( 9, access = 'DIRECT', recl = 20, form = 'FORMATTED' )

      write ( 9, fmt = '( I3 )', rec = 1 ) 128

!
! TestCase 1...
!

      case_id = case_id + 1

      form = '( I4 )'

      call print_msg( form, errmsg(2) )

! Clean up..

      close ( 9, status = 'DELETE' )

     contains

      subroutine print_msg( form_var,  msg_var )

         character(LEN=*), intent(IN)::  form_var
         character(LEN=*), intent(OUT)::  msg_var

         read ( 9, fmt = form_var, rec = 1, iostat = ios, &
        & iomsg = msg_var ) varint1

         write(18,*) msg_var

         if ( ios <> 96 ) call zzrc ( case_id )

      end subroutine

      end                            ! End of TestCase.

