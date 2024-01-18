! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006a5_1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc006a5_1.f
! %VERIFY: falloc006a5_1.out:falloc006a5_1.vf
! %STDIN:
! %STDOUT: falloc006a5_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate unlimited poly allocatable
!                               array with type-spec of derived type)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: id => null()
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,18)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (.not. allocated(x)) error stop 1_4

        print *, 'bounds:', lbound(x,1), ubound(x, 1)

        select type (x)
            class is (base(4))
                call printBaseArray (x)
            class default
                error stop 2_4
        end select
    end subroutine


    subroutine printBaseArray (b1)
        class (base(4)), intent(in) :: b1(:)

        do i = 1, size (b1)
            if (associated (b1(i)%id)) then
                write (*, '(i5)', advance='no') b1(i)%id
            else
                write (*, '(a)', advance='no') 'id not associated'
            end if

            select type (x => b1(i))
                type is (base(4))
                    print *, ''
                type is (child(4,1,*))
                    print *, ', ', x%name
                class default
                    error stop 5_4
            end select
        end do
    end subroutine

    subroutine createX (x, src)
        class(*), intent(out), allocatable :: x(:)
        class (*), intent(in) :: src(:)

        select type (src)
            type is (base(4))
                allocate (base(4) :: x(size(src)))
            type is (child(4,1,*))
                allocate (child(4,1,18) :: x(size(src)))
            class default
                error stop 10_4
        end select
    end subroutine
end module


program falloc006a5_1
use m
    class (*), allocatable :: x1(:)


    call createX (x1, (/(child(4,1,18) (null(), 'abc'), i=1,3)/))

    call printX (x1)

    call createX (x1, (/base(4)()/))

    call printX (x1)
end
