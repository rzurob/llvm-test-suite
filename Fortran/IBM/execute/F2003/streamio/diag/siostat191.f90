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
!*  DESCRIPTION                : RECL= with stream
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat
       character*10 str
       str = 'stream'
       open(unit=11, access=str, recl=4, status='scratch', iostat=iostat, err=100)
       write(11) 'hello'
       close(11)
100    if (iostat /= 191) error stop 1

       open(unit=11, access=str, recl=4, status='scratch')

       end
