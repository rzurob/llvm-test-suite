!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test the PASS attribute; subroutine and
!                               function; manipulation of strings.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string (l)
        integer, len :: l

        character(l) :: val
        procedure(genString), pointer :: concat => null()
        procedure(findNreplace), pointer :: replace => null()
    end type

    contains

    function genString (s1, s2)
        class(string(*)), intent(in) :: s1
        character(*), intent(in) :: s2

        type (string(s1%l+len(s2))) genString

        genString%val = s1%val //s2
    end function

    subroutine findNreplace (s1, from, to)
        class(string(*)), intent(inout) :: s1
        character, intent(in) :: from , to

        integer found

        found = index(s1%val, from)

        do while (found /= 0)
            s1%val(found:found) = to

            found = index(s1%val, from)
        end do
    end subroutine
end module

program dtparamProcComp007
use m
    type(string(:)), allocatable :: s1, s2

    allocate (s1, source=string(10)('xlftest   ', genString, findNreplace))

    call s1%replace(' ', '_')

    allocate (s2, source=s1%concat('team'))

    !! verify s1 and s2

    if ((s1%val /= 'xlftest___') .or. (.not. associated(s1%concat, genString)) &
            .or. (.not. associated(s1%replace, findNreplace))) error stop 1_4

    if ((s2%val /= 'xlftest___team') .or. associated(s2%concat) .or. &
        associated(s2%replace)) error stop 2_4
end
