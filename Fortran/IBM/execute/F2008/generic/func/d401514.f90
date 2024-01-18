program generic_allocatable_pointer_f003
implicit none
   interface alloc_ptr_func
      function i_alloc_func(x) result(res_i_alloc_func)
         integer (KIND=4), DIMENSION(3) :: res_i_alloc_func
         integer (KIND=4), DIMENSION(:),allocatable :: x
      end  function
      function i_ptr_func(x) result(res_i_ptr_func)
         integer (KIND=4), DIMENSION(3) :: res_i_ptr_func
         integer (KIND=4), DIMENSION(:), pointer :: x
      end  function
   end interface

     integer (KIND=4), DIMENSION(3)               :: i_res
     integer (KIND=4), DIMENSION(3), target       :: i_tgt /1, -3, 5/
     integer (KIND=4), DIMENSION(:), allocatable  :: i_alloc
     integer (KIND=4), DIMENSION(:), pointer      :: i_ptr

     integer (KIND=4), DIMENSION(3)               :: i_resp /1, -3, 5/
     integer (KIND=4), DIMENSION(3)               :: i_resa /900, 800, 700/
     allocate(i_alloc(3), source = (/900, 800, 700/))
     i_ptr => i_tgt

     i_res = alloc_ptr_func(i_alloc)
     if (ANY(i_res /= i_resa)) stop 3
     i_res = alloc_ptr_func(i_ptr)
     if (ANY(i_res /= i_resp)) stop 4
     print *, " .... end"
end program

      function i_alloc_func(x) result(res_i_alloc_func)
         implicit none
         integer (KIND=4), DIMENSION(3) :: res_i_alloc_func
         integer (KIND=4), DIMENSION(:),allocatable :: x
         print *, "   inside i_alloc_func()  x=",x
         res_i_alloc_func = x
      end  function
      function i_ptr_func(x) result(res_i_ptr_func)
         implicit none
         integer (KIND=4), DIMENSION(3) :: res_i_ptr_func
         integer (KIND=4), DIMENSION(:), pointer :: x
         print *, "   inside i_ptr_func()  x=",x
         res_i_ptr_func = x
      end  function

