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
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : REWIND with No Error conditions to check if the
!*                               iomsg specifier remains unchanged. the iomsg
!*                               specifier in REWIND statement was passed in
!*                               using a derived type variable.
!*
!*  TEST CONDITIONS            : 1) REWIND sequential file with No Error cond.
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
           character(300)  errmsg

        END TYPE message

      END MODULE message_mod

      program fxiomsgl005

      use message_mod

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      TYPE ( message )  t_msg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      t_msg%errmsg = 'abc'

      open ( 9, access = 'SEQUENTIAL', err = 10 )

      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'

      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'

      rewind ( 9, iostat =ios, iomsg = t_msg%errmsg )

      if ( t_msg%errmsg <> 'abc' ) call zzrc ( case_id )

! Clean up...

      close ( 9, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
