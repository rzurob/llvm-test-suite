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
!*  DESCRIPTION                : Test unformatted stream IO of integers
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer*4 i, j

open(11, access='stream', form='unformatted', action='readwrite', status='new')

write(11) 23
write(11) 87
write(11, pos=30) 84

rewind 11

read(11) i, j
if (i /= 23) error stop 1
if (j /= 87) error stop 2
read(11, pos=30) i
if (i /= 84) error stop 3

inquire(11, pos=i, size=j)
if (i /= 34) error stop 4
if (j /= 33) error stop 5

close(11, status='delete')
end

