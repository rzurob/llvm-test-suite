! F2003/dtparam/TCext/OO_type/abstract/C503/d337764.f
! for defect 337764

module m
type :: base(k1)
    integer, kind :: k1
end type

interface
    type(base) function f()	! should be: type(base(4)) function f()
        import base
    end function
end interface
end module

end
