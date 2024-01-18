!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: slash.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Stream Access I/O
!*
!*  PROGRAMMER                 : Rafik Zurob
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Slash edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890
@process free(f90)

byte ic1, ic2
integer pos, size

open(unit=11, status='replace', form="formatted", access="stream")

! Verify initial file size and position
inquire(11, pos=pos, size=size)
if (pos /= 1) error stop 1
if (size /= 0) error stop 2

! Use only the slash edit descriptor with advancing IO, and verify
! file size and position
write(11, fmt='(/)')
inquire(11, pos=pos, size=size)
if (pos /= 3) error stop 3
if (size /= 2) error stop 4

close(11, status='keep')

! Verify file contents using unformatted stream IO
open(11, status='old', access='stream')
read(11) ic1, ic2
if (ic1 /= 10) error stop 5
if (ic2 /= 10) error stop 6

close(11, status='delete')

end
