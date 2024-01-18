! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg037d.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2005
!*
!*  DESCRIPTION                : argument association (diagnostic: multiple
!                               declaration of a dummy procedure)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name
    end type

    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data(:) => null()

        contains

        final :: finalizeContainer
    end type

    interface
        subroutine copyCo1fromCo2 (co1, co2)
        import container
            class(container(4)), intent(out) :: co1
            class(container(4)), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeContainer (co)
        type (container(4)), intent(inout) :: co

        if (associated(co%data)) deallocate (co%data)
    end subroutine

    subroutine copyData (co1, co2, assgn)
        class(container(4)), intent(out) :: co1
        class(container(4)), intent(in) :: co2

        procedure(copyCo1fromCo2) assgn

        interface assignment(=)
            subroutine assgn (co1, co2)
            import container
                class(container(4)), intent(out) :: co1
                class(container(4)), intent(in) :: co2
            end subroutine
        end interface

        procedure(copyCo1fromCo2) assgn

        procedure(assgn) assgn

        co1 = co2
    end subroutine
end module

program fArg037d

end
