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
!*  DESCRIPTION                : EOF testing
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character*1 c
integer pos

open(unit=11, access='direct', recl=2, status='replace')
write(11, rec=1) 'a\n'
close(11, status='keep')

open(unit=11, access='stream', form='formatted', status='old')
read(unit=11, fmt='(A1)', advance='no') c
if (c /= 'a') error stop 1
inquire(11,pos=pos)
if (pos /= 2) error stop 2

read(11, 20, advance='no') ! A big no-op
20 FORMAT()
inquire(11,pos=pos)
if (pos /= 2) error stop 3

close(11, status='delete')

end
