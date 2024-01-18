! F2003/dtparam/TCext/OO_type/abstract/C503/d337818.f
! for defect 337818

module m
type :: base(k1)
   integer, kind :: k1
end type

interface
   type(base(4)) function f()
      import
   end function
end interface
end module

end
