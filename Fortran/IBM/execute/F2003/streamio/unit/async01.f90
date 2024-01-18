!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: async01.f
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
!*  DESCRIPTION                : Asynchronous Stream IO
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character*3 c3
character*6 c6
integer pos
integer id1, id2
character*36 c36

open(11, access='stream', status='scratch', action='readwrite', asynch='yes')

write(11) '1234567890abcdefghijklmnopqrstuvwxyz'
rewind 11

read(11, id=id1) c3
read(11, id=id2, pos=1) c6

wait(11, id=id1)
wait(11, id=id2)
inquire(11, pos=pos)

if (c3 /= '123') error stop 1
if (c6 /= '123456') error stop 2
if (pos /= 7) error stop 3

write(11, id=id1, pos=11) 'ABCDEFG'
write(11, id=id2, pos=18) 'HIJKLMNOP'

wait(11, id=id1)
wait(11, id=id2)

read(11, pos=1) c36
if (c36 /= '1234567890ABCDEFGHIJKLMNOPqrstuvwxyz') error stop 4
end
