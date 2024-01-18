!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_1_0.f
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  Variation of arrsec_1_0.f with:
!*
!*  - Type parameters of PDT components:
!*    - length type parameter in array size
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k,n)
        integer, kind :: k
        integer, len :: n
        integer(k) :: idarr(n)
        complex(k*2) :: x(k)
        integer(k) :: id
        character(n*2) :: str
        integer :: arr(k:k*2-1) = [(i,i=1,k)]
    end type
end module

use m
integer, parameter :: K = 2
integer, parameter :: L = 2
integer, parameter :: N = 5
type(base(K,:)), allocatable, target :: dtarrobj(:)
type(base(K,:)), pointer :: dtarrptr(:)
allocate(base(K,L) :: dtarrobj(N))
dtarrptr => dtarrobj

do i=1,L
    dtarrobj(1:N:1)%idarr(i) = i*3
end do

dtarrobj(1:N:1)%id = [(i,i=1,N)]
dtarrobj(1:N:1)%str = 'abcd'
call sub1('sub1-a',dtarrobj)
call sub1('sub1-b',dtarrptr)

call sub2(dtarrobj(:))
call sub2(dtarrptr(:))
call sub2(dtarrobj(2:))
call sub2(dtarrptr(2:))
call sub2(dtarrobj(1::3))
call sub2(dtarrptr(1::3))
call sub2(dtarrobj(2::4))
call sub2(dtarrptr(2::4))
call sub2(dtarrobj(5:1:-1))
call sub2(dtarrptr(5:1:-1))
call sub2(dtarrobj(4:1:-4))
call sub2(dtarrptr(4:1:-4))
call sub2(dtarrobj(5:6:-1))
call sub2(dtarrptr(5:6:-1))

contains

    subroutine sub1(str,dta)
        character(*) :: str
        type(base(K,*)) :: dta(:)

        print *,'in sub1 str=',str

        ! subscript-triplet:
        print *, dta(1:5:1)%id
        print *, dta(:)%id
        print *, dta(1:5:1)%idarr(L)
        print *, dta(:)%str

        dta(:)%id = (/6,7,8,9,10/)
        dta(2)%idarr = (/8,9/)
        dta(4:1:-2)%id = (/1,2/)
        print *, dta(:)%id
        print *, dta(1:5:1)%idarr(L)
        print *, dta(:)%str
        ! empty:
        print *, size(dta(4:1:2)%id)
        print *, dta(4:2:1)%id
        print *, dta(4:2:1)%idarr(1)
        print *, dta(4:2:1)%str

        ! vector-subscript:
        dta((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
        print *,dta%id
        print *,dta(:)%id
        print *,dta(:)%idarr(L)
        print *,dta(:)%str

        dta((/3,1,4,2,5/))%id = (/8,6,9,7,5/)
        print *,dta(:)%id
        print *,dta(:)%idarr(1)
        print *,dta(:)%str
    end subroutine


    subroutine sub2(dta)
        type(base(K,*)) :: dta(11:)

        print *,'in sub2:'
		if (size(dta(11:))==0) then
			return
		end if
        print *, dta(11:)%id
        print *, dta(:)%id
        print *, dta%id
        print *, dta(11)%arr
    end subroutine
end
