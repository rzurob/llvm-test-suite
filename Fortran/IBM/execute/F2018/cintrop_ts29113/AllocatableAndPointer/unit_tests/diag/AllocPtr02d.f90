!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr02d
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : March, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : 26305: C-interop Allocatable/Pointer
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Diagnose the following constraint:
!*  
!*  C516: The ALLOCATABLE or POINTER attribute shall not be specified 
!*  for a default-initialized dummy argument of a procedure that has a
!*  proc-language-binding-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m
        use, intrinsic :: iso_c_binding
        implicit none
        type, bind(c) :: base1
          real(c_float) :: b1_r
        end type
        type, bind(c) :: base2
          real(c_float) :: b2_r = 1.0
        end type
      end module

      use, intrinsic :: iso_c_binding
      use m
      implicit none

      interface
         subroutine s1(a1, a2, a3, a4, a5) bind(c)
            import
            type(base1) :: a1
            type(base1), allocatable :: a2
            type(base2), pointer :: a3(:)
            type(base2), allocatable :: a4
            type(base1), pointer :: a5(:)
         end subroutine
      end interface

      end

      subroutine s1(a1, a2, a3, a4, a5) bind(c)
        use, intrinsic :: iso_c_binding
        use m
        implicit none

        type(base1) :: a1
        type(base1), allocatable :: a2
        type(base2), pointer :: a3(:)
        type(base2), allocatable :: a4
        type(base1), pointer :: a5(:)
        
      end subroutine

