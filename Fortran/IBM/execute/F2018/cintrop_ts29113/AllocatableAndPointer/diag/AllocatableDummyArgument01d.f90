!*********************************************************************
!* ===================================================================
!*
!* DATE                         : January 25, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Fortran Bind(c) procedure called from Fortran
!*                                - Nesting of calls
!*                                   Bind(c) ==> Non-bind(c)
!*                                - contiguous attribute
!* Fortran array:
!*   - dim 1 is number of rows
!*   - dim2 is number of columns
!*   - Column order
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
      use iso_c_binding
      implicit none

      contains
      subroutine contig_sub(arr) bind(c)
        integer, contiguous, pointer :: arr(:,:)
      end subroutine
end module mod
