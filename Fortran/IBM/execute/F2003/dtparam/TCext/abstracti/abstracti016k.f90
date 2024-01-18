!***********************************************************************
!* ===================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!* ===================================================================== 
!*
!*  TEST CASE NAME             : abstracti016k
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti016
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-25 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : specific type bound (use the external
!*                               procedure for the deferred binding)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module modA
    type, abstract :: base
        contains

        procedure(p), deferred :: p1
    end type

    abstract interface
        subroutine p (b)
        import
            class(base), intent(in) :: b
        end subroutine
    end interface
end module

module modB
use modA
    type, extends(base) :: child (kchild_1) ! kchild_1=4
       integer, kind :: kchild_1
        real(kchild_1) data

        contains

        procedure :: p1
    end type

    interface
        subroutine p1 (b)
        import
            class(child(4)), intent(in) :: b ! tcx: (4)
        end subroutine
    end interface
end module

program abstracti016k
use modB
    integer, parameter :: sizeB = 5000
    class(base), allocatable :: b1(:)

    allocate (b1(sizeB), source=(/(child(4)(i), i=1,sizeB)/)) ! tcx: (4)

    do i = 1, sizeB, sizeB/10
        call b1(i)%p1
    end do
end

subroutine p1 (b)
use modB, only: child
    class(child(4)), intent(in) :: b ! tcx: (4)

    write (*, '(f12.2)') b%data
end subroutine


! Extensions to introduce derived type parameters:
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
