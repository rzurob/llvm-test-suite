!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: 64pos2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
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
!*                               a 4-byte variable.
!*                               Also test that inquire can handle
!*                               both 4-byte and 8-byte pos variables
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer*4 ios /0/
integer*4 pos

pos = 41

open(2, access='stream', form='unformatted', status='scratch')
write(2, pos=pos, iostat=ios) 'abc'
if (ios /= 0) error stop 1

inquire(2, pos=pos)
if (pos /= 44) error stop 2

call sub1
end

@process intsize(8)
subroutine sub1
  integer*8 pos64 /0/
  inquire(2, pos=pos64)
  if (pos64 /= 44_8) error stop 3
end subroutine
