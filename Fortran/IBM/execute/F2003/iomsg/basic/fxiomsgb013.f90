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
!*  DESCRIPTION                : Open a file with 'DIRECT' access code, but the
!*                               RECL code is missing.
!*
!*  TEST CONDITIONS            : 1) Open a file with 'DIRECT' access code but
!*                                  RECL is absent.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb013

      implicit none

      integer*4       ios
      integer*4       case_id
      character*300   errmsg
      character*11     access_code

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc( case_id )

!
! TestCase 1..
!
      case_id = case_id + 1

      access_code = 'DIRECT'

      open ( 8, FILE = 'file1', access = access_code, form = 'FORMATTED', &
    & err = 10, iostat = ios, iomsg = errmsg )

      call zzrc( case_id )

10    write ( 18, * ) errmsg

      if (ios .ne.25) error stop 1000
      go to 100

100   close (8, status = 'DELETE')

      end

