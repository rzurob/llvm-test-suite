! F2003/dtparam/future/d341784.f
! Defect 370244 (for 341784)

module m
    type base(k1)
        integer, kind :: k1
    end type

    interface
        class (base(4)) function genBase (b)
            import base
            class (base(4)), intent(in) :: b
        end function
    end interface
end module
end
