!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/d361313_2.f
!*  DATE                       : Feb. 20, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232, defect 361313 (seq 3)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type base(n)
    integer, len :: n
    integer :: a(n)
end type

type dta(n)
    integer, len :: n
    type(base(n+1)) :: dtc
end type

type(dta(:)), allocatable :: y
! type(dta(4)), allocatable :: y   ! no problem if type param is not deferred
allocate(dta(4) :: y)

y%dtc = base(5)(([(i,i=1,5)]))
print *,'y%dtc   =',y%dtc          ! output is missing last element of dtc%a
print *,'y%dtc%a =',y%dtc%a        ! prints fine
end
