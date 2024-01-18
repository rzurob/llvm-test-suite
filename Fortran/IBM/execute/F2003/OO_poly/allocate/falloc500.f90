! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE/DEALLOCATE (if the allocate-obj is not
!*                               deallocated successfully)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program  falloc500
integer*4, pointer :: i

integer*4, target :: j
integer err

j = 10

i => j

deallocate (i, stat = err)

if (.not. associated (i, j)) error stop 1_4
end
