! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal515a14kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal515a14 by Jim Xia)
!*  DATE                       : 2007-11-07 (original: 02/14/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of temp created by
!                               structure constructor in where statement)
!*
!*  KEYWORD(S)                 :
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
        integer(kbase_1), allocatable :: id

        contains

        procedure :: equal => b1EqualB2
        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=10
       integer, len :: lchild_1
        character(lchild_1) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        if (allocated (b%id)) then
            print *, 'deallocate id'

            deallocate (b%id)
        end if
    end subroutine

    elemental logical function b1EqualB2 (b1, b2)
        class (base(4)), intent(in) :: b1, b2 ! tcx: (4)

        !! first compare the base part
        if (.not. same_type_as (b1, b2)) then
            b1EqualB2 = .false.
            return
        end if

        if (allocated (b1%id)) then
            b1EqualB2 = allocated (b2%id)

            if (b1EqualB2) then
                b1EqualB2 = (b1%id == b2%id)
            end if
        else
            b1EqualB2 = .not. allocated (b2%id)
        end if

        !! then test the extended part
        if (b1EqualB2) then
            select type (b1)
                type is (base(4)) ! tcx: (4)
                type is (child(4,*)) ! tcx: (4,*)
                    select type (b2)
                        type is (child(4,*)) ! tcx: (4,*)
                            b1EqualB2 = (b1%name == b2%name)
                        class default
                            b1EqualB2 = .false. !<-- should never be here
                    end select
                class default
                    b1EqualB2 = .false.
            end select
        end if
    end function
end module

program ffinal515a14kl
use m
    class (base(4)), allocatable :: b1(:,:) ! tcx: (4)

    logical results(2,2)

    results = .false.

    allocate (child(4,10) :: b1(2,2)) ! tcx: (4,10)

    allocate (b1(1,1)%id, source=1)
    allocate (b1(2,2)%id, source=1)
    allocate (b1(2,1)%id, source=2)

    select type (b1)
        type is (child(4,*)) ! tcx: (4,*)
            b1(1,1)%name = 'test'
            b1(2,2)%name = 'xlftest'
            b1(2,1)%name = 'test'
        class default
            error stop 101_4
    end select

    where (b1%equal(child(4,10)(1, 'test')))   results = .true. ! tcx: (4,10)

    print *, results
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,10) / declare with (4,*) - 5 changes
