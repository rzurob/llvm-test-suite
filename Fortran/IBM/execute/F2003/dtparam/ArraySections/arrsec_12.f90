!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_1.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Sep 23, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  Allocatable deferred shape array of dt, pass 0 size section as dummy arg,
!*  copy arg to an allocatable, pass to another sub 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
module m
    type base(n)
        integer, len :: n
        integer :: id(n)
    end type
end module

use m
type(base(:)), allocatable :: dtarrobj(:)
allocate(base(3) :: dtarrobj(4))

dtarrobj(:)%id(1) = [(i,i=1,4)]
print *, dtarrobj(:)%id(1)

call sub1(dtarrobj(1:2),'sub1 - 1:2')
call sub1(dtarrobj(2:1),'sub1 - 2:1')

call sub2('sub2 - 1:2)',dtarrobj(1:2))
call sub2('sub2 - 2:1)',dtarrobj(2:1))

contains

    subroutine sub1(dta,str)
        use m
        type(base(*)) :: dta(11:)
        character(*) :: str
        type(base(:)), allocatable :: local(:)

        call sub2(str,dta(11:))

        allocate(local(size(dta(11:))), source=dta(11:))
        call sub2(str,local)
        deallocate(local)
    end subroutine

    subroutine sub2(str,dta)
        use m
        type(base(*)) :: dta(:)
        character(*) :: str
        type(base(:)), allocatable :: local(:)

        print *,'in sub2: size(dta(:))=',size(dta(:)),str
        allocate(local(size(dta(:))), source=dta(:))

         if (size(dta(:))==0) then
             deallocate(local)
             return
         end if
        print *, local(:)%id(1)
        deallocate(local)
    end subroutine
end

