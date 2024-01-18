! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal532ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal532a by Jim Xia)
!*  DATE                       : 2007-11-11 (original: 06/28/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (finalization of the data in FORALL
!                               construct)
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

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) id
    end type

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), pointer :: data(:) ! tcx: (kcontainer_1)

        contains

        final :: finalizeData
    end type

    class (container(4)), allocatable :: co1_m(:) ! tcx: (4)

    contains

    elemental subroutine finalizeData (d)
        type (container(4)), intent(inout) :: d ! tcx: (4)

        integer(4) error

        if (associated (d%data))    deallocate (d%data, stat=error)
    end subroutine
end module

program ffinal532ak
use m
    type (container(4)) co1(3), co2(2,2) ! tcx: (4)
    type (container(4)), parameter :: co_const = container(4)(null()) ! tcx: (4) ! tcx: (4)

    allocate (co1(1)%data(2:3), co1(2)%data(3))

    nullify (co1(3)%data)

    allocate (co1_m(3))

    allocate (co1_m(1)%data(1), co1_m(2)%data(10), co1_m(3)%data(1))

    forall (i=1:3)
        co1(i) = co1_m(i)
    end forall

    do i = 1, 3
        if (.not. associated (co1(i)%data, co1_m(i)%data)) error stop 101_4
    end do


    !! assign co2

    allocate (co2(1,1)%data(2), co2(1,2)%data(3), co2(2,1)%data(4))

    co2(2,2)%data => null()

    forall (i=1:2,j=1:2)
        co2(i,j) = co_const
    end forall

    do i =1,2
        do j = 1,2
            if (associated (co2(i,j)%data)) error stop 2_4
        end do
    end do
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 5 changes
