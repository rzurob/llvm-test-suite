! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type real*16
!*                               TO is of unlimited poly, function name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

        real*16, allocatable, target :: var(:,:)
        real*16, pointer :: p(:,:)
        integer i

        allocate(var(4:7,3:5 ))

        p => var

        p(4, 3:5) = (/ ( real(i, 16), i = 3,5) /)
        p(5, :) = var(4,:) + 5
        p(6, :) = -var(4,:)
        p(7,:) = -p(5,:)

        select type ( x => func())
            type is (real(16))
                write (*, '(4f15.8)') x
            class default
                stop 33
        end select

        contains
            function func()
                   class(*) func
                   allocatable func(:,:)

                   call move_alloc(var, func)
            end function

    end
