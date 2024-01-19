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
!*  DESCRIPTION                : Simple endfile functionality
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character*2 :: c = ''
integer pos, size
open(11, form='formatted', status='scratch', access='stream')
write(11, '(A2,/,A2,/,A2)') 'ab', 'cd', 'ef'
rewind 11
! file contents:  "ab\ncd\nef\n"

read(11, '(A)', pos=7) c
if (c /= 'ef') error stop 1
inquire(11, pos=pos)
print *, pos
if (pos /= 10) error stop 2

endfile 11 ! truncate
! file contents:  "ab\ncd\n"
inquire(11, pos=pos)
if (pos /= 10) error stop 3

backspace 11
backspace 11
inquire(11, pos=pos)
if (pos /= 4) error stop 4

read(11, '(A)', advance='no') c
if (c /= 'cd') error stop 5

endfile 11
! file contents "ab\ncd"
inquire(11, pos=pos, size=size)
if (pos /= 6) error stop 6
if (size /= 5) error stop 7

close(11)
end

