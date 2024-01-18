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
!*  DESCRIPTION                : Very simple formatted stream write testing
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character*4 :: rec = ''

open (11, access='stream', form='formatted', status='new')
write(11, '(A2)', advance='yes') "ab"
write(11, '(A2)', advance='yes') "cd"
write(11, '(A2)', advance='no') "ef"
write(11, '(A2)') "gh"
close(11, status='keep')

open(11, access='sequential', status='old', pad='no')
read(11, *) rec
if (rec /= 'ab') error stop 1
read(11, *) rec
if (rec /= 'cd') error stop 2
read(11, *) rec
if (rec /= 'efgh') error stop 3
backspace 11
endfile 11
close(11)

open(11, access='direct', status='old', recl=3, form='formatted', pad='yes')
read(11, '(A)', rec=2) rec
if (rec /= 'cd\n  ') error stop 4
read(11, '(A)', rec=1) rec
if (rec /= 'ab\n  ') error stop 5
close(11, status='delete')

end

