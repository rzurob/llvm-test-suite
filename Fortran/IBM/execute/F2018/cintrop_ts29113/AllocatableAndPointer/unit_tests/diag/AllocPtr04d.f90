!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : 26305: C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnose the following constraint:
!*
!*  If a dummy argument in an interoperable interface is a pointer,
!*  it must not have the CONTIGUOUS attribute. (See section 8.7 bullet
!*  6-c of the TS)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine mysubroutine(n, p1, p2, p3) bind(c)
            import
            integer(c_int) :: n
            integer(c_int), pointer, contiguous :: p1
            integer(c_int), pointer, contiguous :: p2(:)
            real(c_float), pointer :: p3(:,:)
            contiguous :: p3
         end subroutine

      end interface

      end

      subroutine mysubroutine(n, p1, p2, p3) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int) :: n
        integer(c_int), pointer, contiguous :: p1
        integer(c_int), contiguous :: p2(:)
        pointer :: p2
        real(c_float), pointer :: p3(:,:)
        contiguous :: p3
      end subroutine


