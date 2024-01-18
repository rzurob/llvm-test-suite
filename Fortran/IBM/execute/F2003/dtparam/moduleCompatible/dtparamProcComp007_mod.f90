
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
