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
!*  DESCRIPTION                : Different combinations of invalid commas were
!*                               put in format statements. The WRITE statement
!*                               was tested in a function which returns a
!*                               string.
!*
!*  TEST CONDITIONS            : 1) Invalid comma between parenthesis.
!*                               2) Invalid commas after I format code.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl027

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      integer*4 varint

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

      form = '( 50 (,I5,) )'

      errmsg = print_msg( form )

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( 50 ( I5,, ) )'

      errmsg = print_msg( form )

      write(18,*) errmsg

      contains

      character*300 function print_msg ( form_var )
        character(LEN=*), intent(IN):: form_var
        character*300 errmsg_var

        write(0, fmt = form_var, iostat = ios, iomsg = errmsg_var ) varint

        print_msg = errmsg_var

      end function print_msg

      end                            ! End of TestCase.
