!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (diagnostic: multiple
!                               declaration of a dummy procedure)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(20) name
    end type

    type container
        class(base), pointer :: data(:) => null()

        contains

        final :: finalizeContainer
    end type

    interface
        subroutine copyCo1fromCo2 (co1, co2)
        import container
            class(container), intent(out) :: co1
            class(container), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeContainer (co)
        type (container), intent(inout) :: co

        if (associated(co%data)) deallocate (co%data)
    end subroutine

    subroutine copyData (co1, co2, assgn)
        class(container), intent(out) :: co1
        class(container), intent(in) :: co2

        procedure(copyCo1fromCo2) assgn

        interface assignment(=)
            subroutine assgn (co1, co2)
            import container
                class(container), intent(out) :: co1
                class(container), intent(in) :: co2
            end subroutine
        end interface

        procedure(copyCo1fromCo2) assgn

        procedure(assgn) assgn

        co1 = co2
    end subroutine
end module

program fArg037d

end
