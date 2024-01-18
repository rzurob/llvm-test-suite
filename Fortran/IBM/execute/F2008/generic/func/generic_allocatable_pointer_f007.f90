!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f007.f
!*  TYPE                       : Functional test
!*  FEATURE                    : #917301 F2008: Generic resolution extensions
!*  RTC Master Story           : 17301: F2008: Generic resolution extensions (master story)
!*                               https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/17301
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 29 June 2012
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                :
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type Base
        integer   :: i_base /1/
    end type

    type, extends(Base) :: Child
       integer  :: i_child /2/
    end type
end module

program generic_allocatable_pointer_f007
use m
implicit none
   interface alloc_ptr_sub
      subroutine t_ptr_sub(x,y)
         import :: Base, Child
         class(*), pointer :: x(:)
         type(Child) :: y(:)
      end subroutine
      subroutine t_alloc_sub(x,y)
         import :: Base, Child
         class(*),allocatable :: x(:)
         type(Child) :: y(:)
      end subroutine
   end interface

   interface alloc_ptr_func
      function t_alloc_func(x)
         import :: Base, Child
         type(Child) :: t_alloc_func(2)
         class(*),allocatable :: x(:)
      end function
      function t_ptr_func(x)
         import :: Base, Child
         type(Child) :: t_ptr_func(2)
         class(*), pointer :: x(:)
      end function
   end interface

     type(Child)                       :: t_res(2)
     class(*), allocatable             :: t_alloc(:)
     class(child), allocatable, target :: t_alloc_tgt(:)
     class(*), pointer                 :: t_ptr(:)

     allocate (t_alloc(2), source = (/ child(111,22222), child(33333,44)/) )
     allocate (t_alloc_tgt(2), source = (/ child(555,66666), child(77777,88)/) )
     t_ptr => t_alloc_tgt

     call alloc_ptr_sub(t_alloc, t_res)
     if ((t_res(1)%i_base /= 111) .or. (t_res(1)%i_child /= 22222) .or. &
         (t_res(2)%i_base /= 33333) .or. (t_res(2)%i_child /= 44)) error stop 1
     call alloc_ptr_sub(t_ptr, t_res)
     if ((t_res(1)%i_base /= 555) .or. (t_res(1)%i_child /= 66666) .or. &
         (t_res(2)%i_base /= 77777) .or. (t_res(2)%i_child /= 88)) error stop 1

     t_res = alloc_ptr_func(t_alloc)
     if ((t_res(1)%i_base /= 111) .or. (t_res(1)%i_child /= 22222) .or. &
         (t_res(2)%i_base /= 33333) .or. (t_res(2)%i_child /= 44)) error stop 3
     t_res = alloc_ptr_func(t_ptr)
     if ((t_res(1)%i_base /= 555) .or. (t_res(1)%i_child /= 66666) .or. &
         (t_res(2)%i_base /= 77777) .or. (t_res(2)%i_child /= 88)) error stop 4
     print *," .... end"
end program

      subroutine t_alloc_sub(x,y)
      use m
      implicit none
         class(*),allocatable :: x(:)
         type(Child) :: y(:)
         select type (x)
            class is (base)
               error stop 11
            class is (child)
               y = x 
               print *, "   inside t_alloc_sub()  y=",y
            class default
               error stop 12
         end select
      end subroutine

      subroutine t_ptr_sub(x,y)
      use m
      implicit none
         class(*), pointer :: x(:)
         type(Child) :: y(:)
         select type (x)
            class is (base)
               error stop 13
            class is (child)
               y = x
               print *, "   inside t_ptr_sub()  y=",y
            class default
               error stop 14
         end select
      end subroutine

      function t_alloc_func(x)
      use m
      implicit none
         class(*),allocatable :: x(:)
         type(Child) :: t_alloc_func(2)
         select type (x)
            class is (base)
               error stop 15
            class is (child)
               t_alloc_func = x
               print *, "   inside t_alloc_func()  t_alloc_func=",t_alloc_func
            class default
               error stop 16
         end select
      end function

      function t_ptr_func(x)
      use m
      implicit none
         class(*), pointer :: x(:)
         type(Child) :: t_ptr_func(2)
         select type (x)
            class is (base)
               error stop 17
            class is (child)
               t_ptr_func = x
               print *, "   inside t_ptr_func()  t_ptr_func=",t_ptr_func
            class default
               error stop 18
         end select
      end function
