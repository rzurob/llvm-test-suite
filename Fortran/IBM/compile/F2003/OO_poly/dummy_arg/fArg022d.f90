!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fArg022d.f
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 08/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (array section with vector
!                               subscript is NOT definable; and shall not be
!                               used as the actual argument to be associated
!                               with a dummy argument with INTENT(OUT))
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
    type base
        integer*4 :: id

        contains

        procedure, non_overridable :: assgnId => assignID2Base
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    contains

    elemental subroutine assignID2Base (b, id)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine
end module

program fArg022d
use m
    interface assignment(=)
        subroutine base2Base (b1, b2)
        use m
            type (base), intent(out) :: b1(:)
            class (base), intent(in) :: b2(:)
        end subroutine
    end interface

    class (base), allocatable :: b1(:)

    type (child) :: c1(4)
    integer vec(4)

    allocate (b1(size(c1)), source=(/child(1, 'b1_1'), child(2, 'b1_2'), &
                child(3,'b1_3'), child(4,'b1_4')/))


    !

    c1 = (/child(40, 'c1_static_1'), child(30, 'c1_static_2'), &
                    child(20,'c1_static_3'), child(10, 'c1_static_1')/)

    vec = (/4,3,2,1/)

    b1 (vec) = c1     !<-- illegal
end

subroutine base2Base (b1, b2)
use m
    type (base), intent(out) :: b1(:)
    class (base), intent(in) :: b2(:)

    call b1%assgnID(b2%id)
end subroutine
