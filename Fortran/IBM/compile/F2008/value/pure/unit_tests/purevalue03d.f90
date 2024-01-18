!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue03d.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - declare dummy procedure with value attribute
!*  - also test with nested functions and subroutines
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

contains
pure integer function foo (x, pf, ps, y, ef, es, z)
    integer, value :: x
    interface
        pure integer function pf (x)
            integer, value :: x
        end function pf

        pure subroutine ps (npf, x)
            interface
                pure integer function npf (x)
                    integer, value :: x
                end function npf
            end interface
            value npf
            integer, intent (inout) :: x
        end subroutine ps

        elemental integer function ef (x)
            integer, value :: x
        end function ef

        elemental subroutine es (x)
            integer, value :: x
        end subroutine es

    end interface
    value :: pf,ps,ef,es
    integer, value :: y,z
    foo = x
end function foo

pure subroutine soo (x, pf, ps, y, ef, es, z)
    integer, value :: x
    interface
        pure integer function pf (x)
            integer, value :: x
        end function pf

        pure subroutine ps (npf, x)
            interface
                pure integer function npf (x)
                    integer, value :: x
                end function npf
            end interface
            value npf
            integer, intent (inout) :: x
        end subroutine ps

        elemental integer function ef (x)
            integer, value :: x
        end function ef

        elemental subroutine es (x)
            integer, value :: x
        end subroutine es
    end interface
    value :: pf,ps,ef,es,y,z
end subroutine soo

end
