!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : A file is created and connected to a unit, then
!*                               open again with a different unit number, this
!*                               will cause an recoverable I/O  error.
!*
!*  TEST CONDITIONS            : 1) Create and connect a file to one unit, then
!*                                  open again with another unit number.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb011

      implicit none

      integer*4       ios

      integer*4       case_id

      character*300   errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc( case_id )

!
! TestCase 1..
!
      case_id = case_id + 1

      open ( 8, FILE = 'file1', err = 10, iostat = ios )

      open ( 9, FILE = 'file1', err = 10, iostat = ios, iomsg = errmsg )

      call zzrc( case_id )

10    write ( 18, * ) errmsg

      if (ios .ne.23) error stop 1000
      go to 100

100   close (8, status = 'DELETE')

      end

