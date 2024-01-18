!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #917301 F2008: Generic resolution extensions
!*  RTC Master Story           : 17301: F2008: Generic resolution extensions (master story)
!*                               https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/17301
!*
!*  DATE                       : 29 June 2012
!*
!*  REQUIRED COMPILER OPTIONS  : -qarch=auto
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                :
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

program generic_allocatable_pointer_f002
implicit none
   interface alloc_ptr_sub
      subroutine v_alloc_sub(x,y)
         vector(unsigned(4)),allocatable ::x
         vector(unsigned(4)) y
      end  subroutine
      subroutine v_ptr_sub(x,y)
         vector(unsigned(4)), pointer ::x
         vector(unsigned(4)) y
      end  subroutine
      subroutine b_alloc_sub(x,y)
         byte,allocatable ::x
         byte y
      end  subroutine
      subroutine b_ptr_sub(x,y)
         byte, pointer ::x
         byte y
      end  subroutine

   end interface
   interface alloc_ptr_func
      vector(unsigned(4)) function v_alloc_func(x)
         vector(unsigned(4)),allocatable ::x
      end  function
      vector(unsigned(4)) function v_ptr_func(x)
         vector(unsigned(4)), pointer::x
      end  function
      byte function b_alloc_func(x)
         byte,allocatable ::x
      end  function
      byte function b_ptr_func(x)
         byte, pointer::x
      end  function

   end interface

     vector(unsigned(4))               :: v_res
     vector(unsigned(4)), target       :: v_tgt
     vector(unsigned(4)), allocatable  :: v_alloc
     vector(unsigned(4)), pointer      :: v_ptr

     vector(unsigned(4))               :: v_tmp1
     vector(unsigned(4))               :: v_tmp2
     integer(4)                        :: i_res(4)
     integer(4)                        :: i_tmp1(4)
     integer(4)                        :: i_tmp2(4)
     integer(4)                        :: i_resa(4)
     integer(4)                        :: i_resp(4)

     byte               :: b_res
     byte, target       :: b_tgt /55/
     byte, allocatable  :: b_alloc
     byte, pointer      :: b_ptr

     equivalence (v_res, i_res)
     equivalence (v_tmp1, i_tmp1)
     equivalence (v_tmp2, i_tmp2)
     i_res  = 0
     i_tmp1 = (/1,2,3,4/)
     i_tmp2 = (/99,88,77,66/)
     i_resa = (/99, 176, 231, 264/)
     i_resp = (/100, 90, 80, 70/)

!------------------  Type: vector(unsigned(4))
     v_tgt = vec_add (v_tmp1, v_tmp2)
     allocate(v_alloc)
     v_alloc = vec_mul (v_tmp2, v_tmp1)
     v_ptr => v_tgt

     call alloc_ptr_sub(v_alloc, v_res)
     if (ANY(i_res /= i_resa) ) error stop 1
     call alloc_ptr_sub(v_ptr, v_res)
     if (ANY(i_res /= i_resp) ) error stop 2
     v_res = alloc_ptr_func(v_alloc)
     if (ANY(i_res /= i_resa) ) error stop 3
     v_res = alloc_ptr_func(v_ptr)
     if (ANY(i_res /= i_resp) ) error stop 4

!------------------  Type: byte
     allocate(b_alloc)
     b_alloc = -10
     b_ptr => b_tgt

     call alloc_ptr_sub(b_alloc, b_res)
     if (b_res /= -10) error stop 1
     call alloc_ptr_sub(b_ptr, b_res)
     if (b_res /= 55) error stop 2
     b_res = alloc_ptr_func(b_alloc)
     if (b_res /= -10) error stop 3
     b_res = alloc_ptr_func(b_ptr)
     if (b_res /= 55) error stop 4

end program

      subroutine v_alloc_sub(x,y)
         implicit none
         vector(unsigned(4)),allocatable ::x
         vector(unsigned(4)) y
         y = x
         print *, "   inside v_alloc_sub()  "
      end  subroutine
      subroutine v_ptr_sub(x,y)
         implicit none
         vector(unsigned(4)), pointer ::x
         vector(unsigned(4)) y
         y = x
         print *, "   inside v_ptr_sub()  "
      end  subroutine
      vector(unsigned(4)) function v_alloc_func(x)
         implicit none
         vector(unsigned(4)),allocatable ::x
         v_alloc_func = x
         print *, "   inside v_alloc_func()  "
      end  function
      vector(unsigned(4)) function v_ptr_func(x)
         implicit none
         vector(unsigned(4)), pointer::x
         v_ptr_func = x
         print *, "   inside v_ptr_func()  "
      end  function

      subroutine b_alloc_sub(x,y)
         implicit none
         byte,allocatable ::x
         byte y
         y = x
         print *, "   inside b_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine b_ptr_sub(x,y)
         implicit none
         byte, pointer ::x
         byte y
         y = x
         print *, "   inside b_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      byte function b_alloc_func(x)
         implicit none
         byte,allocatable ::x
         print *, "   inside b_alloc_func()  x=",x
         b_alloc_func = x
      end  function
      byte function b_ptr_func(x)
         implicit none
         byte, pointer::x
         print *, "   inside b_ptr_func()  x=",x
         b_ptr_func = x
      end  function
