! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2005
!*
!*  DESCRIPTION                : argument association (array constructor
!                               containing unlimited poly entities used as
!                               actual-arg)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    class (*) function genData (data)
        allocatable :: genData(:)

        class (*), intent(in) :: data(:)

        allocate (genData (size(data)), source=data)
    end function

    subroutine printX (x)
        class (*), intent(in) :: x(:)

        select type (x)
            type is (integer(8))
                print *, x
            type is (real(8))
                write (*, '(10f8.2)') x
            type is (character(*))
                write (*, '(10(1x, a))') x
            class default
                print *, 'other type'
        end select
    end subroutine
end module

program fArg521a
use m
    real (8) :: r1(10)

    r1 = (/(j*1.1d0, j=1,10)/)

    call printX ((/genData((/1_8, 3_8, 5_8, 7_8/)), genData((/2_8, 4_8, 6_8/))/))

    call printX ((/genData(r1(::3)), genData (r1((/2, 5/)))/))

    call printX ((/genData ((/'abc', 'XYZ'/)), genData ((/'ibm', 'xlf'/))/))
end
