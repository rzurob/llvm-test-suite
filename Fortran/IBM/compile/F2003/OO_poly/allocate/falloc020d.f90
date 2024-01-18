!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : ALLOCATE/DEALLOCATE (nullify() and deallocate()
!                               can only apply on variables)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc020d
    interface
        integer(4) function f()
            pointer :: f
        end function

        class (*) function g()
            allocatable g
        end function

        class (*) function h(i)
            pointer h(:)
        end function
    end interface

    nullify (f())       !<-- this is illegal

    deallocate (f())    !<-- this is illegal

    deallocate (g())    !<-- this is illegal

    nullify (h(10))     !<-- this is illegal
    deallocate (h(100)) !<-- this is illegal

    end

    integer(4) function f()
        pointer :: f

        allocate (f, source= 100)
    end function

    class (*) function g()
        allocatable g

        allocate (g, source= 100)
    end function

    class (*) function h(i)
        pointer h(:)

        h => null()
    end function
