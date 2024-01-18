!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl017.f
! %VERIFY:
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
!*  DESCRIPTION                : READ with No Error conditions to check if the
!*                               iomsg specifier remains unchanged. The iomsg
!*                               specifier was passed in by a derived type
!*                               variable which is a member of an array of
!*                               another derived type.
!*
!*  TEST CONDITIONS            : 1) READ sequential file with No Error cond.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl017

      implicit none                     ! All variables must be Declared

      TYPE message

           integer(4)      useless
           character(300)  errmsg

      END TYPE message

      TYPE message2

           integer          useless2
           TYPE ( message ) errmsg (5)

      END TYPE message2

      integer*4 case_id                 ! Test Case id under test.

      integer*4 varint, ios

      character*20 form

      TYPE ( message2 )  t_msg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
!  OPEN file to read from
!

      open ( 9, access = 'SEQUENTIAL', err = 10 )

      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'

      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'

      rewind 9

!
! TestCase 1...
!

      case_id = case_id + 1

      t_msg%errmsg(3)%errmsg = 'abc'

      form = '( A5 )'

      read ( 9, fmt = form, iostat = ios, iomsg = t_msg%errmsg(3)%errmsg, &
  &   end = 10 ) varint

      if ( t_msg%errmsg(3)%errmsg <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 9, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
