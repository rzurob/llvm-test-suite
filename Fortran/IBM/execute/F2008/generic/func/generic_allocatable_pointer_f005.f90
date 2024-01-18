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
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                :
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type Base
        integer   :: i_b
        character :: c_b
    end type
end module

program generic_allocatable_pointer_f005
use m
implicit none
   interface alloc_ptr_sub
      subroutine t_ptr_sub(x,y)
         import :: Base
         type(Base), pointer :: x
         type(Base) :: y
      end subroutine
      subroutine t_alloc_sub(x,y)
         import :: Base
         type(Base),allocatable :: x
         type(Base) :: y
      end subroutine
   end interface

   interface alloc_ptr_func
      type(Base) function t_alloc_func(x)
         import :: Base
         type(Base),allocatable :: x
      end function
      type(Base) function t_ptr_func(x)
         import :: Base
         type(Base), pointer :: x
      end function
   end interface

     type(Base)               :: t_res
     type(Base), target       :: t_tgt
     type(Base), allocatable  :: t_alloc
     type(Base), pointer      :: t_ptr

     t_tgt%i_b = 44444
     t_tgt%c_b = 'O'
     allocate(t_alloc)
     t_alloc%i_b = 77777
     t_alloc%c_b = 'z'
     t_ptr => t_tgt

     call alloc_ptr_sub(t_alloc, t_res)
     if ((t_res%i_b /= 77777) .or. (t_res%c_b /= 'z')) error stop 1
     call alloc_ptr_sub(t_ptr, t_res)
     if ((t_res%i_b /= 44444) .or. (t_res%c_b /= 'O')) error stop 2
     t_res = alloc_ptr_func(t_alloc)
     if ((t_res%i_b /= 77777) .or. (t_res%c_b /= 'z')) error stop 3
     t_res = alloc_ptr_func(t_ptr)
     if ((t_res%i_b /= 44444) .or. (t_res%c_b /= 'O')) error stop 4

end program

      subroutine t_alloc_sub(x,y)
      use m
      implicit none
         type(Base),allocatable :: x
         type(Base) :: y
         y = x
         print *, "   inside t_alloc_sub()  x=",x,"  y=",y
      end subroutine
      subroutine t_ptr_sub(x,y)
      use m
      implicit none
         type(Base), pointer :: x
         type(Base) :: y
         y = x
         print *, "   inside t_ptr_sub()  x=",x,"  y=",y
      end subroutine
      type(Base) function t_alloc_func(x)
      use m
      implicit none
         type(Base),allocatable :: x
         print *, "   inside t_alloc_func()  x=",x
         t_alloc_func = x
      end function
      type(Base) function t_ptr_func(x)
      use m
      implicit none
         type(Base), pointer :: x
         print *, "   inside t_ptr_func()  x=",x
         t_ptr_func = x
      end function

