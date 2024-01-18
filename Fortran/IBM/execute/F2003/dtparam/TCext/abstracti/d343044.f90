!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: No Finalization for Polymorphic
!*                               Temporary used as Actual Argument
!*
!*  DESCRIPTION                :
!*  The Reduced Code below creates a temporary (ALLOCATABLE) instance of
!*  the Derived Type "triangle", which is passed as the Actual Argument
!*  to the SUBROUTINE "printArea()".  This temporary instance should be
!*  Finalized when the execution of "printArea()" has completed.
!*
!*  When Derived Type Parameters are removed from the code, this temporary
!*  instance is Finalized.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    type :: triangle(k2)    ! (4)
        integer, kind :: k2
        integer(k2)   :: inst
        contains
        procedure :: genShape => genTriangle
        final :: finalizeTriangle
    end type

    contains

    subroutine printArea (s)
        class (triangle(4)), intent(in) :: s
        print *, 'printArea', s%inst
    end subroutine

    class (triangle(4)) function genTriangle (s)
        class (triangle(4)), intent(in) :: s
        allocatable genTriangle
        allocate (genTriangle, source=s)
        select type ( genTriangle )
            class is ( triangle(4) )
                genTriangle%inst = genTriangle%inst + 10
        end select
    end function

    subroutine finalizeTriangle (t)
        type (triangle(4)), intent(inout) :: t

        print *, 'finalizeTriangle', t%inst
    end subroutine

end module

program d343044
use m
    type (triangle(4)) :: t1 = triangle(4) (1)

    t1%inst = 2
    call printArea(t1%genShape())

end program d343044
