! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : June. 20, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block should
!*                              : not be defined twice.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  07/16/04   KV     - added dummy main prog, so TC would pass on Linux
!*                      (see defect 285281 for more detail)
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer x
common /blk/ x
bind(c, name="foo") /blk/
bind(c, name="foo") /blk/
end module
program bcdatw02
end program
