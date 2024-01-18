!*  ============================================================================
!*
!*  DATE                       : 2011-02-18
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
!*  Copying/moving data from image one to other images.
!*  Type used logical, character, integer, real and complex
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   implicit none
   integer x, y

   contains

   subroutine asc_L(A, SIZE)
      logical, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do x = 1, SIZE
         do y = 1, SIZE
            if (A(x, y) .NEQV. A(x, y)[1]) then
               print *,this_image(),": SUB:asc_L: ",A(x, y)," .NE. ",A(x, y)[1]
               ERROR STOP 101
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_L

   subroutine asc_C(A, SIZE)
      character, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      character :: ch
      do x = 1, SIZE
         do y = 1, SIZE
            ch = A(x, y)[1]
            if (iachar(A(x, y)) .NE. iachar(ch)) then
               print *,this_image(),": SUB:asc_C: ",A(x, y)," .NE. ",ch
               ERROR STOP 102
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_C

   subroutine asc_I(A, SIZE)
      integer, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do x = 1, SIZE
         do y = 1, SIZE
            if (A(x, y) .NE. A(x, y)[1]) then
               print *,this_image(),": SUB:asc_I: ",A(x, y)," .NE. ",A(x, y)[1]
               ERROR STOP 103
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_I

   subroutine asc_R(A, SIZE)
      real, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do x = 1, SIZE
         do y = 1, SIZE
            if (A(x, y) .NE. A(x, y)[1]) then
               print *,this_image(),": SUB:asc_R: ",A(x, y)," .NE. ",A(x, y)[1]
               ERROR STOP 104
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_R


   subroutine asc_COMPLEX(A, SIZE)
      complex, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do x = 1, SIZE
         do y = 1, SIZE
            if (A(x, y) .NE. A(x, y)[1]) then
               print *,this_image(),": SUB:asc_COMPLEX: ",A(x, y)," .NE. ",A(x, y)[1]
               ERROR STOP 105
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_COMPLEX

end module asc

program main

   use asc

   implicit none
   integer, parameter :: SIZE = 10
   integer A, xm, ym

   logical, dimension(SIZE, SIZE), save :: L[*]
   character, dimension(SIZE, SIZE), save :: C[*]
   integer, dimension(SIZE, SIZE), save :: I[*]
   real, dimension(SIZE, SIZE), save :: R[*]
   complex, dimension(SIZE, SIZE), save :: CMPLEX[*]

   !
   ! Initialization of the data
   !
   if (this_image() .EQ. 1) then
      A = iachar('A') - num_images()
      do xm = 1, SIZE
         do ym = 1, SIZE
            if (xm + ym .LE. num_images()) then
               L(xm, ym) = .TRUE.
            end if
            C(xm, ym) = achar(A + xm + ym + num_images())
            I(xm, ym) = xm + ym + num_images()
            R(xm, ym) = xm + ym + num_images() * 2.5
            CMPLEX(xm, ym) = (xm + ym + num_images() * 2.5, xm + ym + num_images() * 1.5)
         end do
      end do
   end if

   SYNC ALL

   if (this_image() .NE. 1) then
      L(:,:) = L(:,:)[1]
      C(:,:) = C(:,:)[1]
      I(:,:) = I(:,:)[1]
      R(:,:) = R(:,:)[1]
      CMPLEX(:,:) = CMPLEX(:,:)[1]
   end if

   call asc_L(L, SIZE);
   call asc_C(C, SIZE);
   call asc_I(I, SIZE);
   call asc_R(R, SIZE);
   call asc_COMPLEX(CMPLEX, SIZE);

end program main
