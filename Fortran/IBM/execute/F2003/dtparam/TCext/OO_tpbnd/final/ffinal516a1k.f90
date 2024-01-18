! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal516a1k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal516a1 by Jim Xia)
!*  DATE                       : 2007-11-11 (original: 02/16/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of the structure
!                               constructor used in the specification
!                               expression)
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
    type base (lbase_1) ! lbase_1=3
       integer, len :: lbase_1
        integer(selected_int_kind(3)), allocatable :: id(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(*)), intent(inout) :: b ! tcx: (*)

        if (allocated (b%id)) then
            print *, 'deallocating id of', size(b%id), 'elements'

            deallocate (b%id)
        end if
    end subroutine

    pure integer function arraySize (b)
        class (base(*)), intent(in) :: b ! tcx: (*)

        if (allocated(b%id)) then
            arraySize = size (b%id)
        else
            arraySize = 0
        end if
    end function
end module

program ffinal516a1k
use m
    class (*), allocatable :: x

    integer array1(3)

    array1 = (/1,2,3/)

    allocate (character(4*arraySize(base(3)(array1))) :: x) ! tcx: (3)

    print *, 'after allocating'

    select type (x)
        type is (character(*))
            if (len (x) /= 12) error stop 101_4

            write (x, '(3i4)') array1

            write (*, '(a)') x
        class default
            error stop 4_4
    end select
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
