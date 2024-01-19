! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (source= specifier appears more than
!                               once)
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

program falloc005d2
    class (*), pointer :: x(:)

    character(200) error
    integer stat1

    !! the following allocate specifies source= twice
    allocate (x(2), source=100_8, errmsg=error, stat=stat1, source=1.0)
end
