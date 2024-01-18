! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal011allk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal011a by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (private parent type will be
!                               finalized)
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
    type, private :: base (lBase) ! lBase=0
        integer, len :: lBase
        class (*), pointer :: data => null()
        contains

        final :: finalizeBase
    end type

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character (lchild_1) name
    end type

    private finalizeBase

    contains

    subroutine finalizeBase (b)
        type (base(*)), intent(inout) :: b ! tcx: (*)

        if (associated (b%data)) then
            print *, 'deallocating data'

            deallocate (b%data)
        end if
    end subroutine

    elemental subroutine assgnVal (c, data, name)
        class (child(*,*)), intent(inout) :: c ! tcx: (*,*)
        class (*), intent(in) :: data
        character(*), intent(in) :: name

        allocate (c%data, source=data)

        c%name = name
    end subroutine
end module

module m1
use m
    type, extends(child) :: gen3 (kgen3_1) ! kgen3_1=8
       integer, kind :: kgen3_1
        integer (kgen3_1) id
    end type
end module

program ffinal011allk
use m1
    class (child(:,:)), pointer :: c1, c2(:) ! tcx: (:,:)

    allocate (gen3(0,20,8) :: c1, c2(2)) ! tcx: (0,20,8)

    call assgnVal (c1, 1.5e0_8, 'abc')

    call assgnVal (c2, 'ibm', (/'xyz', 'abc'/))

    print *, 'test 1'
    deallocate (c1)

    print *, 'test 2'

    deallocate (c2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lBase) to invoke with (0) / declare with (*) - 1 changes
! type: child - added parameters (lchild_1) to invoke with (0,20) / declare with (*,*) - 2 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (0,20,8) / declare with (*,*,8) - 1 changes
