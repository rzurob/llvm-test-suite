module some
   use ISO_C_BINDING, ONLY : C_INT, C_BOOL, C_FLOAT, C_PTR, C_CHAR, C_LOC, C_ASSOCIATED, C_F_POINTER, C_FUNPTR, C_FUNLOC
contains
    subroutine subce(a,b,c) bind(c)
      integer(C_INT),value :: a
      integer(C_INT),value :: b
      real(C_FLOAT),value :: c
      print *, "Hello world subce called with ", a
      print *, b
      print *, c
    end subroutine subce

end module some

program fxisopca01
   use ISO_C_BINDING, ONLY : C_INT, C_BOOL, C_FLOAT, C_PTR, C_CHAR, C_LOC, C_ASSOCIATED, C_F_POINTER, C_FUNPTR, C_FUNLOC
   use some
   interface
      subroutine sub4(x) bind(c)
         use ISO_C_BINDING, ONLY : C_FUNPTR
         type(C_FUNPTR),value:: x
      end subroutine sub4
   end interface

   type(C_PTR) :: cp
   type(C_FUNPTR) :: psubc
   !procedure(subce) :: proc_subc
   psubc = C_FUNLOC(subce)

   call sub4(psubc)

   contains
    subroutine subc(a) bind(c)
      INTEGER(C_INT) :: a(18, 3:7, 2)
      print *, "Hello world subc called with "
      print *, a(2,4,2)
    end subroutine subc

end program fxisopca01