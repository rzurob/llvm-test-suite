! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July 28, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common blocks work with
!*                              : -qextchk
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  06/28/04    KV    - corrected incorrect ineroperability of "character(1) z"
!*                      with "char z[1]" - defect 285281
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program bcunchk2
integer x
real y
character z(1)
common /blk/ x, y, z
bind(c, name="bar") :: /blk/
x = 1
y = 2.0
z = 'F'
print *, x, y, z
call csub()
print *, x, y, z
end program
