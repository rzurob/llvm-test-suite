!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr03d
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
!*  If a dummy argument in an interoperable interface is of type
!*  CHARACTER and is allocatable or a pointer, its character length
!*  shall be deferred.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine s1(cc1, cc2, cc3, cc4, cc5) bind(c)
            import
            character(:), pointer :: cc1
            character(5), pointer :: cc2
            character(*), pointer :: cc3
            character(5), allocatable :: cc4
            character(*), allocatable :: cc5
         end subroutine
         subroutine s2(cc1, cc2, cc3, cc4, cc5) bind(c)
            import
            character(:), pointer :: cc1(:)
            character(5), pointer :: cc2(:)
            character(*), pointer :: cc3(:)
            character(5), allocatable :: cc4(:)
            character(*), allocatable :: cc5(:)
         end subroutine

      end interface

      end

      subroutine s1(cc1, cc2, cc3, cc4, cc5) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        character(:), pointer :: cc1
        character(5), pointer :: cc2
        character(*), pointer :: cc3
        character(5), allocatable :: cc4
        character(*), allocatable :: cc5
      end subroutine

      subroutine s2(cc1, cc2, cc3, cc4, cc5) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        character(:), pointer :: cc1(:)
        character(5), pointer :: cc2(:)
        character(*), pointer :: cc3(:)
        character(5), allocatable :: cc4(:)
        character(*), allocatable :: cc5(:)
      end subroutine
