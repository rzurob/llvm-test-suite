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
!*  DESCRIPTION                : Invalid POS= value
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat, z
       z = -1
       open(unit=11, access='stream', status='scratch')
       write (11, pos=z, iostat=iostat) 10
       if (iostat /= 195) error stop 1

       z = -20
       write (11, pos=z) 10
       end
