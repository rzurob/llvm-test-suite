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
!*  DESCRIPTION                : Special handling of Comma when reading
!*                               numbers.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer*4 i1, i2, pos

! Create a file containing Comma-Separated Values.
open(11, access='stream', file='csv', status='replace')
write(11) '  2  ,  3,  4,  5\n'
close(11)

open(11, file='csv', form='formatted', access='stream', status='old')

! Read "  2  " as an integer.
read(11, '(i5)', advance='no') i1
if (i1 /= 2) error stop 1
inquire(11, pos=pos)
if (pos /= 6) error stop 2

! Read ",  3," as an integer.  Should stop at the first comma
read(11, '(i5)', advance='no') i2
inquire(11, pos=pos)
if (pos /= 6) error stop 3

! Read "  3,  4,  " as an integer.  Should stop at the "," between 3 and 4.
read(11, '(i10)', pos=7, advance='no') i2
if (i2 /= 3) error stop 4
inquire(11, pos=pos)
if (pos /= 10) error stop 5

close(11, status='delete')
end

