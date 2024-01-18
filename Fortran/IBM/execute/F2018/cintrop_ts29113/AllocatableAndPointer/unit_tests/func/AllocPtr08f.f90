!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr08f
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
!*  DESCRIPTION                : Calling TIFs that end up turning into
!*                               calls to the RTE.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(ptr1, ptr2, cc, carr) bind(c)
          import
          integer(c_int), pointer :: ptr1(:,:), ptr2(:)
          character(:), allocatable :: cc
          character(:), pointer :: carr(:)
        end
      end interface

      integer(c_int), pointer :: ptr1(:,:), ptr2(:)
      character(:), allocatable :: cc
      character(:), pointer :: carr(:)

      ptr1 => NULL()
      ptr2 => NULL()

      allocate(character(1)::carr(10))
      carr = ['i', 'c', 'j', 'd', 'e', 'f', 'g', 'h', 'a', 'b']
      call test(ptr1, ptr2, cc, carr)

      end

      subroutine test(a, b, c, carr) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        ! Arguments
        integer(c_int), pointer :: a(:, :)
        integer(c_int), pointer :: b(:)
        character(:), allocatable :: c
        character(:), pointer :: carr(:)

        ! Locals
        integer :: i
        integer(c_int), target :: t1(3,2)

        ! ****** Test 1: ****** !

        t1 = reshape([(-i, i=1,6,1)], [3,2])
        a => t1
        allocate(b(6))
        b = transfer(a, b)

        print *, b

        ! ****** Test 2: ****** !
        allocate(character(5)::c)
        c = "abc  "
        print *, c, "."
        print *, trim(c), "."
        deallocate(c)


        ! ****** Test 3: ****** !
        a(3,1) = 0
        
        print *, maxloc(a)
        print *, carr
        print *, maxloc(carr)

    end subroutine

