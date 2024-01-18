!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray31.f
!*
!*  DATE                       : 2011-02-24
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments assumed shape
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 386330
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the assumed shape dummy arguments coarrays
!*
!*  Uses same extent dummy arguments but different bounds
!*  Type used real
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   implicit none
   integer :: row, col

   contains

   subroutine passArray1(A, SIZE, value)
      integer, intent(in) :: SIZE
      real, intent(in) :: value
      real, dimension(SIZE/2:), intent(in) :: A[*]
      do row = SIZE/2, SIZE
         if (A(row) .NE. value) then
            print *,this_image(),":",A(row)," .NE. ",value
            ERROR STOP 102
         end if
      end do
      print *,this_image(),":",A
   end subroutine passArray1

   subroutine passArray2(A, SIZE, value)
      integer, intent(in) :: SIZE
      real, intent(in) :: value
      real, dimension(SIZE/2:,SIZE/2:), intent(in) :: A[*]
      do row = SIZE/2, SIZE
         do col = SIZE/2, SIZE
            if (A(row,col) .NE. value) then
               print *,this_image(),":",A(row,col)," .NE. ",value
               ERROR STOP 104
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine passArray2

end module asc

program main

   use asc
   implicit none

   integer, parameter :: SIZE = 10
   real :: value1, value2
   real, save :: A1(SIZE)[*]
   real, save :: A2(SIZE,SIZE)[*]

   value1 = 23.4567
   value2 = 19.4567
   A1(:) = value1
   A2(:,:) = value2

   call passArray1(A1, SIZE, value1)
   call passArray2(A2, SIZE, value2)

end program main
