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
!*  DESCRIPTION                : stream IO on a unit that isn't open
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat, i
       write(11, pos=11, iostat=iostat) 6
       if (iostat /= 198) error stop 1

       read(11, pos=11) i
       end

