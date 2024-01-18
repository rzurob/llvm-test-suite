!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_1_20.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jul. 09, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  Variation of arrsec_1_10.f with:
!*  - Array sections of a polymorphic PDT array as dummy arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
module m
    type base(k,n)
        integer, kind :: k
        integer, len :: n
        integer(k) :: idarr(n)
        complex(k*2) :: x(k)
        integer(k) :: id
        character :: str(n)
        integer :: arr(k:k*2-1)
    end type
end module

use m
integer, parameter :: K = 2
integer, parameter :: L = 2
integer, parameter :: N = 5
integer, parameter :: SUB2LB = 11
integer, parameter :: SUB2UB = (SUB2LB+N-1)
class(base(K,:)), allocatable, target :: dtarrobj(:)
class(base(K,:)), pointer :: dtarrptr(:)
allocate(base(K,L) :: dtarrobj(SUB2UB))
dtarrptr => dtarrobj

!do i=1,L
!    dtarrobj(1:SUB2UB:1)%idarr(i) = i*3
!end do

dtarrobj(1:SUB2UB:1)%id = [(i,i=1,SUB2UB)]
dtarrobj(1:SUB2UB:1)%arr(2) = 1
dtarrobj(1:SUB2UB:1)%arr(3) = 2

call sub1('sub1-a',N,dtarrobj)
call sub1('sub1-b',N,dtarrptr)

call sub2(SUB2UB,dtarrobj(:))
call sub2(SUB2UB,dtarrptr(:))
call sub2(SUB2UB,dtarrobj(2:))
call sub2(SUB2UB,dtarrptr(2:))
call sub2(SUB2UB,dtarrobj(1::3))
call sub2(SUB2UB,dtarrptr(1::3))
call sub2(SUB2UB,dtarrobj(5:1:-1))
call sub2(SUB2UB,dtarrptr(5:1:-1))

contains

    subroutine sub1(str,ub,dta)
        character(*) :: str
        integer :: ub
        class(base(K,*)) :: dta(*)

        print *,'in sub1 str=',str

        ! subscript-triplet:
        print *, dta(1:ub:1)%id
        print *, dta(:ub)%id
        
        dta(:ub)%id = (/6,7,8,9,10/)
        dta(4:1:-2)%id = (/1,2/)
        print *, dta(:ub)%id
        ! empty:
        print *, size(dta(4:1:2)%id)
        print *, dta(4:2:1)%id
        
        ! vector-subscript:
        dta((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
        print *,dta(:ub)%id
        
        dta((/3,1,4,2,5/))%id = (/8,6,9,7,5/)
        print *,dta(:ub)%id
    end subroutine


    subroutine sub2(ub,dta)
        integer :: ub
        class(base(K,*)) :: dta(SUB2LB:*)

        print *,'in sub2:'
        print *, dta(SUB2LB:ub)%id
        print *, dta(:ub)%id
        print *, dta(SUB2LB)%arr
    end subroutine
end
