! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (IO during select type construct)
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

program fselTyp511
    type base
        class (*), pointer :: data(:)
    end type

    class (base), allocatable :: b1(:)

    write (1, *) 1, 2, 3

    rewind (1)

    allocate (b1(2))

    allocate (integer :: b1(1)%data(3))

    select type (x => b1(1)%data)
        type is (integer)
            if (size(x) /= 3) error stop 1_4

            read (1,*) x

            print *, x

            if (any (x /= (/1,2,3/))) error stop 2_4
        class default
            error stop 4_4
    end select

    close (1, status='delete')
end
