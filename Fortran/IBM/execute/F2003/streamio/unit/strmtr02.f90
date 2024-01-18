!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: strmtr02.f
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
!*  DESCRIPTION                : T, TL, TR edit descriptors
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character*12 c

open(11, status='scratch', access='stream', form='formatted')

write(11, '(T5, A1, TL3, TR5, A5)') 'a', 'bcdef'

write(11, '(T5, A1, TL3)', advance='no') 'a'
write(11, '(TR5, A5)') 'bcdef'

write(11, '(T5, A1, TL3, A1, TR4, A5)') 'a', 'b', 'cdefg'

write(11, '(A1)', pos=40+4, advance='no') 'a'
write(11, '(TR5, A5)', pos=40+2) 'bcdef'

read(11, '(A)', pos=1) c
if (c /= "    a  bcdef") error stop 1
read(11, '(A)') c
if (c /= "    a  bcdef") error stop 2
read(11, '(A)') c
if (c /= "  b a  cdefg") error stop 3

end
