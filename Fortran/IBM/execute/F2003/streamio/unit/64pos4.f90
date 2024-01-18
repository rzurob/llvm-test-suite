!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test that the POS= specifier can be
!*                               a 4-byte constant.
!*                               Also test inqiure with 4-byte and
!*                               8-byte POS= variables.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      integer*4, parameter :: q = 41
      integer*4 pos32

      open(20, access='stream', status='scratch')
      write(20, pos=q) "ab"

      inquire(20, pos=pos32)
      if (pos32 /= 43) error stop 1

      call sub1
      end

@process intsize(8)
      subroutine sub1
        integer*8 pos64
        inquire(20, pos=pos64)
        if (pos64 /= 43_8) error stop 2
      end subroutine

