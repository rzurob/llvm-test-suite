!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: 64pos.f
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
!*                               an 8-byte variable.
!*                               (The value of the variable fits in
!*                               32-bits to avoid needing a LARGEFILE
!*                               volume.  An analogous test case for
!*                               files >2GB can also be found in the
!*                               bigfiles bucket.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer*4 ios /0/
integer*8 pos
integer*4 pos32 /0/

pos = 41_8

open(2, access='stream', form='unformatted', status='scratch')
write(2, pos=pos, iostat=ios) 'abc'
if (ios /= 0) error stop 1

inquire(2, pos=pos32)
if (pos32 /= 44_4) error stop 2

call sub1
end

@process intsize(8)
subroutine sub1
  integer*8 pos64 /0/
  inquire(2, pos=pos64)
  if (pos64 /= 44_8) error stop 3
end subroutine
