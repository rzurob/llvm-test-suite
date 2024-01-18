use, intrinsic :: iso_c_binding
type(c_funptr) :: cptr

interface
  subroutine sub() bind(c)
  end

  subroutine sub2()
  end
end interface

cptr = c_funloc(sub)
cptr = c_funloc(sub2)

end
