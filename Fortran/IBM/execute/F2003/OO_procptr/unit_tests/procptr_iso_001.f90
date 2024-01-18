       !
       ! Use C_F_PROCPOINTER module proceudre.
       !

       use, intrinsic :: iso_c_binding
       interface
         real function foo() bind(c)
         end function
       end interface

       type(c_funptr) :: aaa
       real mmm, nnn 
       type dd
         procedure(real), nopass, pointer :: pp1
         procedure(real), nopass, pointer :: pp2
       end type
       type(dd) :: ddobj

       aaa = c_funloc(foo)
       call c_f_procpointer(aaa, ddobj%pp1)

       ddobj%pp2 => foo

       mmm = ddobj%pp1()
       nnn = ddobj%pp2()
       
       if (mmm .ne. nnn) stop 11 
       end
       
       real function foo()
          foo = 1.34
       end function
