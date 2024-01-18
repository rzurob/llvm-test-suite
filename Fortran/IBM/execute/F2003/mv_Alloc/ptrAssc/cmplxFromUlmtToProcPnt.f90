! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.FROM is of type complex(4)
!*                               2.TO and pointer are of type class(*)
!*                               3. move_alloc appears in an external proc
!*                                  which is the interface proc of a procedure
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program main

        interface
            subroutine sub(a1, a2)
                complex(4), allocatable :: a1(:)
                class(*), allocatable :: a2(:)
            end subroutine
        end interface

        class(*), pointer :: p(:)
        class(*), target, allocatable :: a1(:)
        complex*8, target, allocatable :: c1(:)
        logical precision_x8

        procedure(sub), pointer ::  pp1

        pp1 => sub

        allocate(c1(2), source=(/cmplx(1.d1,1.d0,4),cmplx(2.d1,1.d0,4)/))

        p => c1

        call pp1(c1, a1)

        if ( .not. associated(p, a1)) stop 2

        select type ( p )
            type is (complex*8)
                 if ( size(p) /= 2) stop 11
                 if ( .not. precision_x8 (p(1), (10.0000,1.0000))) &
                            error stop 21_4
                 if ( .not. precision_x8 (p(2),(20.0000,1.0000))) &
                            error stop 31_4
            class default
                 stop 41
        end select
end program


subroutine sub(a1, a2)
    complex(4), allocatable :: a1(:)
    class(*), allocatable :: a2(:)

    call move_alloc(a1, a2)
end subroutine

