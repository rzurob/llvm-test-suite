!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_1.f
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  Variation of arrsec_1_0.f with:
!*  - Array section dummy args as actual arg to other subroutines
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k,n)
        integer, kind :: k
        integer, len :: n
        character :: str(n)
        integer(k) :: idarr(n)
        complex(k*2) :: x(k)
        integer(k) :: id
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

!do i=1,L
!    dtarrobj(1:N:1)%idarr(i) = i*3
!end do

dtarrobj(1:N:1)%id = [(i,i=1,N)]
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
        call sub1_doit(str,dta)
    end subroutine

    subroutine sub1_doit(str,dta)
        character(*) :: str
        type(base(K,*)) :: dta(:)

        print *,'in sub1 str=',str

        ! subscript-triplet:
        print *, dta(1:5:1)%id
        print *, dta(:)%id

        dta(:)%id = (/6,7,8,9,10/)
        dta(4:1:-2)%id = (/1,2/)
        print *, dta(:)%id
        ! empty:
        print *, size(dta(4:1:2)%id)
        print *, dta(4:2:1)%id

        ! vector-subscript:
        dta((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
        print *,dta%id
        print *,dta(:)%id

        dta((/3,1,4,2,5/))%id = (/8,6,9,7,5/)
        print *,dta(:)%id
    end subroutine


    subroutine sub2(dta)
        type(base(K,*)) :: dta(11:)
        call sub2_doit(dta(11:))
    end subroutine

    subroutine sub2_doit(dta)
        type(base(K,*)) :: dta(11:)

        print *,'in sub2:'
        ! subscript-triplet:
        print *, dta(11:)%id
        print *, dta(:)%id
        print *, dta%id
        print *, dta(11)%arr
    end subroutine
end
