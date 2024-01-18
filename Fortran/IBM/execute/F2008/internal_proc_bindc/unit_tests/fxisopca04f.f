module some
contains
    subroutine subce() bind(c)
      print *, "Hello world subce"
    end subroutine subce
end module some

program fxisopca01
   use ISO_C_BINDING, ONLY : C_PTR, C_CHAR, C_LOC, C_ASSOCIATED, C_F_POINTER, C_FUNPTR, C_FUNLOC
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
   psubc = C_FUNLOC(subc) 
   
   call sub4(psubc)

   contains
    subroutine subc() bind(c, name="thisShouldNotWork")
      print *, "Hello world subc"
    end subroutine subc

end program fxisopca01
