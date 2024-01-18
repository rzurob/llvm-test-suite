!######################################################################i
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_d001.f
!*  TYPE                       : Diagnostic test
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

module f2008_generic
implicit none

   type Base
      integer :: i /-1/
      contains
         procedure, nopass :: ptr_sub
         generic :: alloc_ptr_sub => ptr_sub

         procedure, pass :: add_alloc
         generic :: operator(+) => add_alloc

         procedure, pass :: assgn_alloc
         generic :: assignment(=) => assgn_alloc
   end type

   type, extends(Base) :: Child
      integer :: j /8/
      contains
         procedure, nopass :: alloc_sub
         generic :: alloc_ptr_sub => alloc_sub

         procedure, pass :: add_ptr
         generic :: operator(+) => add_ptr

         procedure, pass :: assgn_ptr
         generic :: assignment(=) => assgn_ptr
   end type

   contains

      subroutine ptr_sub(x, y)
      implicit none
         class(Base) :: x
         class(Base), pointer :: y

         print *, "   inside ptr_sub()"
      end subroutine
      subroutine alloc_sub(x, y)
      implicit none
        class(Child) :: x
        class(Child) , allocatable :: y

        print *, "   inside alloc_sub()"
      end subroutine


      type(Base) function add_alloc(x, y)
      implicit none
         class(Base), intent(in) :: x
         class(Base), intent(in), pointer :: y
         integer   :: z

         add_alloc = Base(100)
         print *, "   inside add_alloc()  "
      end function
      type(Base) function add_ptr(x, y)
      implicit none
        class(Child), intent(in) :: x
        class(Child), intent(in), allocatable :: y

         add_ptr = Base(300)
         print *, "   inside add_ptr()  "
      end function

      subroutine assgn_alloc(x, y)
         class(Base), intent(out) :: x
         class(Base), allocatable, intent(in)  :: y
         print *,"   inside assgn_alloc"
      end subroutine

      subroutine assgn_ptr(x, y)
         class(Child), intent(out) :: x
         class(Child), pointer, intent(in)  :: y
         print *,"   inside assgn_ptr"
      end subroutine

end module

program generic_allocatable_pointer_d001
use f2008_generic
implicit none

   print *," .... end"
end program
