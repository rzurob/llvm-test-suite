!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
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
!*                               specifier in READ statement was passed in using
!*                               a derived type variable.
!*
!*  TEST CONDITIONS            : 1) READ sequential file with No Error cond.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl004

      implicit none                     ! All variables must be Declared

        TYPE message
           integer(4)      useless
           character(300)  errmsg
        END TYPE message

        TYPE message2
           integer          useless2
           TYPE ( message ) errmsg
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

      t_msg%errmsg%errmsg = 'abc'

      form = '( A5 )'

      read ( 9, fmt = form, iostat = ios, iomsg = t_msg%errmsg%errmsg, &
   &  end = 10 ) varint

      if ( t_msg%errmsg%errmsg <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 9, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
