!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr12f
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
!*  DESCRIPTION                : Transpose
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(al1) bind(c)
          import
          integer(c_int), allocatable :: al1(:,:)
        end
      end interface

      integer(c_int), allocatable :: al1(:,:)
      integer :: i
      allocate(al1(5, 5))
      al1 = reshape([(-i, i=1,5*5,1)], [5, 5])

      call test(al1)

      end

      subroutine test(a) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        ! Arguments
        integer(c_int), allocatable :: a(:,:)

        ! ****** Test 1: ****** !
        print *, "~~~ Test 1:"
        print *, a
        a = transpose(abs(a))
        print *, a

        ! ****** Test 2: ****** !
        print *, "~~~ Test 2:"
        a = transpose(a)
        print *, a
        
    end subroutine

