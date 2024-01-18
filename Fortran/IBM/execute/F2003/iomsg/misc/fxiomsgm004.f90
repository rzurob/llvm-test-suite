!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : WRITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The IOMSG option should be in effective where
!*                               it's specified. This test case  uses the option
!*                               IOMSG in Test Case1 while not in Test Case2,
!*                               check if IOMSG is still effective in Test Case2.
!*
!*  TEST CONDITIONS            : 1) WRITE  on a SYN I/O file with IOMSG option
!*                             : 2) WRITE  on a SYN I/O file without IOMSG
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgm004

      implicit none                     ! All variables must be Declared

      integer*4 case_id, i, j           ! Test Case id under test.
      integer*4 ios, wid(2)
      character*300 errmsg
      logical*4  there

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = 'abc'

      open ( 8, form = 'unformatted', err = 10, access = 'STREAM', asynch='NO' )

      write ( 8, id = wid(1), err = 20, iostat = ios, iomsg = errmsg ) 'CCCCC'

      call zzrc ( case_id )

20    write ( 18, * ) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      errmsg = 'abc'

      open ( 9, form = 'unformatted', err = 10, access = 'STREAM', asynch='NO' )

      write ( 9, id = wid(2), err = 30, iostat = ios ) 'DDDDD'

      call zzrc ( case_id )

30    write ( 18, * ) errmsg

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.

