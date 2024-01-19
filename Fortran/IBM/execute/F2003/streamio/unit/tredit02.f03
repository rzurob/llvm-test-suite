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
!*  DESCRIPTION                : TR edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890

character*10 :: c = ''
integer pos, size
open(11, access='stream', form='formatted', status='scratch')
write(11, '(A3,TR3,A3)') "abc", "def"
inquire(11, pos=pos, size=size)
if (pos /= 11) error stop 1
if (size /= 10) error stop 2

rewind 11

read(11, '(A9)') c    ! padding
if (c /= 'abc   def ') error stop 3

backspace 11  ! Will take us to position 1

read(11, '(A2,X)', advance='no') c
if (c /= 'ab        ') error stop 4
inquire(11, pos=pos)
if (pos /= 4) error stop 5  ! i.e. X will cause us to advance one character.

endfile 11  ! truncate
inquire(11, pos=pos, size=size)
if (pos /= 4) error stop 6
if (size /= 2) error stop 7  ! inconsistency: size /= pos-1

rewind 11
read(11, *) c
if (c /= 'ab') error stop 8

close(11)

end
