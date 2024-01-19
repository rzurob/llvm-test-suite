!
! F2003/dtparam/misc/d369658_1.f
! Per defect 369658
!
module m
    type base(n)
        integer, len :: n
        integer :: id
    end type
end module

use m
! OK:
!type(base(2)) :: dtarrobj(3)

! fail:
type(base(:)), allocatable :: dtarrobj(:)
allocate(base(2) :: dtarrobj(3))

dtarrobj(:)%id = [(i,i=1,3)]
print *,dtarrobj%id
call sub1(3,dtarrobj)

contains

    subroutine sub1(ub,dta)
        integer :: ub
        type(base(*)) :: dta(*)
        print *, dta(1:ub)%id
        print *, dta(:ub)%id
    end subroutine
end

