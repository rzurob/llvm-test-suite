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
!*  DESCRIPTION                : Langlvl checking of ACCESS= specifier
!*                               when its value is 'stream'.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer iostat
character*10 stream
stream='stream'

call setrteopts("langlvl=95std")

open(11, access=stream, status='scratch', iostat=iostat)
close(11)
if (iostat /= 199) error stop 1

open(11, access=stream, status='scratch')  ! Error message
close(11)

end
