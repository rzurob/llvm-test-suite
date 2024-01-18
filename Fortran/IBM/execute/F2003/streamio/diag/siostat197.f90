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
!*  DESCRIPTION                : POS= on non-seekable unit
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat
       open(11, file='/dev/null', access="stream")
       write(11, pos=3, iostat=iostat) 10
       if (iostat /= 197) error stop 1

       write(11, pos=3) 10
       end

