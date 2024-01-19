! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/19/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 321737)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    character(:), allocatable :: c(:)

    contains

    function genStr (i)
        character(35) genStr

        write (genStr, '(DC,a,";",e10.2,";",e10.2)') 'xlf', cmplx(i, i)
    end function
end module

program decEditDesc004
use m
    open (1, form='formatted', access='stream')

    write (1, 200) (genStr(j), j = 1, 100)

    rewind 1

    call readVars (100, 1)

    call verifyVars

    close(1)

200 format (DC, 1x, 100a)

end


subroutine readVars (isize, unit)
use m
    integer, intent(in) :: isize, unit

    if (allocated (c)) deallocate (c)

    allocate (character(35) :: c(isize))

    read (unit, '(dc, 1x, 100a)') c
end subroutine


subroutine verifyVars
use m
    do i = 1, 100
        if (c(i) /= genStr(i)) error stop 2_4
    end do
end subroutine
