! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/28/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 339000)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k,n)
        integer, kind :: k
        integer, len :: n
        type(base(k,*)), pointer :: next => null()   ! '*' is illegal here
    end type
end module
end