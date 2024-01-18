!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: ufmtreal01.f
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
!*  DESCRIPTION                : Test unformatted stream IO of real values
!*
!234567890123456789012345678901234567890123456789012345678901234567890
real*4 r
real*4 rx
integer*4 ix
equivalence(rx, ix)

r = z"7ffff000"

! Unformatted stream access
open(11, access='stream', form='unformatted', action='readwrite', status='new')

write(11) r

rewind 11

read(11) rx
if (ix /= z"7ffff000") error stop 1

close(11, status='keep')

! Verify using direct access
rx = 0.0
open(11, access='direct', form='unformatted', recl=4, status='old')
read(11, rec=1) rx
if (ix /= z"7ffff000") error stop 2

close(11, status='delete')
end
