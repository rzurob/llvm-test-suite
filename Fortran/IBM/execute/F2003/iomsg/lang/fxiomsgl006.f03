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
!*                               iomsg specifier in BACKSPACE was passed in
!*                               using a derived type variable.
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

      program fxiomsgl006

      implicit none

      TYPE message

           integer(4)      useless
           character(300)  errmsg

      END TYPE message

      TYPE message2

           integer          useless2
           TYPE ( message ) errmsg

      END TYPE message2

      integer*4 case_id

      integer*4 ios, unit_number

      TYPE( message2 ) t_msg

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

      backspace ( unit = unit_number, iostat = ios, iomsg=t_msg%errmsg%errmsg,&
   &  err = 10 )

      call zzrc ( case_id )

 10   write( 18, * ) t_msg%errmsg%errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      open ( 8, access = 'DIRECT', recl = 80 )

      backspace ( unit = 8, iostat = ios, err = 30, iomsg=t_msg%errmsg%errmsg)

      call zzrc ( case_id )

 30   write(18, *) t_msg%errmsg%errmsg

      if ( ios <=0 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      open ( 9, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

      backspace ( unit = 9, iostat = ios, err = 40 ,iomsg=t_msg%errmsg%errmsg)

      call zzrc ( case_id )

 40   write(18, *) t_msg%errmsg%errmsg

      if ( ios <=0 ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      end            ! End of TestCase.
