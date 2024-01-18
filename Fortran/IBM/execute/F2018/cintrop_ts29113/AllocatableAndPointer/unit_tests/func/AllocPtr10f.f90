!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr10f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : CFI interaction with stubs
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(ptr) bind(c)
          import
          integer(c_int), pointer :: ptr(:,:)
        end
      end interface

      integer(c_int), pointer :: ptr(:,:)

      ptr => NULL()

      call test(ptr)

      end

      subroutine test(a) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        interface
           subroutine foo(a)
             import
             integer(c_int) :: a(10, 5)
           end
        end interface

        ! Arguments
        integer(c_int), pointer :: a(:, :)

        ! Locals
        integer :: i, j
        integer(c_int), pointer :: b(:,:)
        integer(c_int), target :: t1(10,5)
        integer(c_int), target :: t2(20,10)

        ! ****** Test 1: ****** !
        t1 = reshape([(i, i=1,50,1)], [10,5])
        a => t1
        print *, is_contiguous(a)
        call foo(a)

        ! ****** Test 2: ****** !
        t2 = reshape([(i, i=1,200,1)], [20,10])
        a => t2(::2, ::2)
        print *, is_contiguous(a)
        call foo(a)

        ! ****** Test 3: ****** !
        b => t1
        print *, is_contiguous(b)
        call foo(b)

        ! ****** Test 4: ****** !
        b => t2(::2, ::2)
        print *, is_contiguous(b)
        call foo(b)

    end subroutine

    subroutine foo(a)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in) :: a(10,5)
      print *, a
    end
