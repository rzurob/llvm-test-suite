!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl029.f
! %VERIFY: fort.18:fxiomsgl029.vf
! %STDIN: 
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f message_mod.mod
! %END
!***************************************************************************
 

!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            : Negative reptition counter in FORMAT
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : WRITE  FORMAT
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Negative reptition counter in FORMAT is not 
!*                               allowed in I/O statements. The WRITE statement
!*                               was tested in an interface.
!*
!*  TEST CONDITIONS            : 1) Negative repetition counter for I format cd.
!*                               2) Negative repetition counter for X format cd.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************


      MODULE message_mod
     
      contains

      subroutine print_msg( form_var,  msg_var )
         character(LEN=*), intent(IN)::  form_var
         character(LEN=*), intent(OUT)::  msg_var

         write(0, fmt = form_var, iostat = ios, iomsg=msg_var ) varint

         write(18,*) msg_var

      end subroutine  print_msg


      subroutine print_dummy1( intvar )
        integer  intvar

        write(0, *)  "dummy1"

      end subroutine print_dummy1


      subroutine print_dummy2()

        write(0, *)  "dummy2" 

      end subroutine print_dummy2

      end module message_mod


      program fxiomsgl029

      use message_mod
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
      integer*4 varint/100/
 
      character*20 form

      character*300 errmsg

      interface face_msg
        module procedure print_msg, print_dummy1, print_dummy2
     end interface
 
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
 
      call face_msg( form, errmsg )

 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      form = '( -50 ( 5X ) )'
 
      call face_msg( form, errmsg )

      end                            ! End of TestCase.
