!*  ============================================================================
!*
!*  DATE                       : 2011-02-10
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
!*  This program contains modules that help in testing the assumed shape
!*  dummy arguments coarrays
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc_L

   implicit none
   integer xl, yl, zl

   contains

   subroutine asc_L_1(A)
      logical, dimension(:), intent(in) :: A[*]
      do xl = 1, SIZE(A)
         if (A(xl) .NEQV. (xl .LE. num_images())) then
            print *,this_image(),": SUB:asc_L_1: ",A(xl)," .NE. ",xl .LE. num_images()
            ERROR STOP 101
         end if
      end do
      print *,this_image(),":",A
   end subroutine asc_L_1

   subroutine asc_L_2(A, SIZE)
      logical, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do xl = 1, SIZE
         do yl = 1, SIZE
            if (A(xl, yl) .NEQV. (xl + yl .LE. num_images())) then
               print *,this_image(),": SUB:asc_L_2: ",A(xl, yl)," .NE. ",xl + yl .LE. num_images()
               ERROR STOP 102
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_L_2

   subroutine asc_L_3(A, SIZE)
      logical, dimension(:,:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do xl = 1, SIZE
         do yl = 1, SIZE
            do zl = 1, SIZE
               if (A(xl, yl, zl) .NEQV. (xl + yl + zl .LE. num_images())) then
                  print *,this_image(),": SUB:asc_L_3: ",A(xl, yl, zl)," .NE. ",xl + yl + zl .LE. num_images()
                  ERROR STOP 103
               end if
            end do
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_L_3

end module asc_L

module asc_C

   implicit none
   integer Ac, xc, yc, zc

   contains

   subroutine asc_C_1(A)
      character, dimension(:), intent(in) :: A[*]
      Ac = iachar('A') - num_images()
      do xc = 1, SIZE(A)
         if (A(xc) .NE. achar(Ac + xc + num_images())) then
            print *,this_image(),": SUB:asc_C_1: ",A(xc)," .NE. ",achar(Ac + xc + num_images())
            ERROR STOP 101
         end if
      end do
      print *,this_image(),":",A
   end subroutine asc_C_1

   subroutine asc_C_2(A, SIZE)
      character, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      Ac = iachar('A') - num_images()
      do xc = 1, SIZE
         do yc = 1, SIZE
            if (A(xc, yc) .NE. achar(Ac + xc + yc + num_images())) then
               print *,this_image(),": SUB:asc_C_2: ",A(xc, yc)," .NE. ",achar(Ac + xc + yc + num_images())
               ERROR STOP 102
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_C_2

   subroutine asc_C_3(A, SIZE)
      character, dimension(:,:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      Ac = iachar('A') - num_images()
      do xc = 1, SIZE
         do yc = 1, SIZE
            do zc = 1, SIZE
               if (A(xc, yc, zc) .NE. achar(Ac + xc + yc + zc + num_images())) then
                  print *,this_image(),": SUB:asc_C_3: ",A(xc, yc, zc)," .NE. ",achar(Ac + xc + yc + zc + num_images())
                  ERROR STOP 103
               end if
            end do
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_C_3

end module asc_C

module asc_I

   implicit none
   integer xi, yi, zi

   contains

   subroutine asc_I_1(A)
      integer, dimension(:), intent(in) :: A[*]
      do xi = 1, SIZE(A)
         if (A(xi) .NE. xi + num_images()) then
            print *,this_image(),": SUB:asc_I_1: ",A(xi)," .NE. ",xi + num_images()
            ERROR STOP 101
         end if
      end do
      print *,this_image(),":",A
   end subroutine asc_I_1

   subroutine asc_I_2(A, SIZE)
      integer, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do xi = 1, SIZE
         do yi = 1, SIZE
            if (A(xi, yi) .NE. xi + yi + num_images()) then
               print *,this_image(),": SUB:asc_I_2: ",A(xi, yi)," .NE. ",xi + yi + num_images()
               ERROR STOP 102
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_I_2

   subroutine asc_I_3(A, SIZE)
      integer, dimension(:,:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do xi = 1, SIZE
         do yi = 1, SIZE
            do zi = 1, SIZE
               if (A(xi, yi, zi) .NE. xi + yi + zi + num_images()) then
                  print *,this_image(),": SUB:asc_I_3: ",A(xi, yi, zi)," .NE. ",xi + yi + zi + num_images()
                  ERROR STOP 103
               end if
            end do
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_I_3

end module asc_I

module asc_R

   implicit none
   integer xr, yr, zr

   contains

   subroutine asc_R_1(A)
      real, dimension(:), intent(in) :: A[*]
      do xr = 1, SIZE(A)
         if (A(xr) .NE. xr + num_images() * 1.5) then
            print *,this_image(),": SUB:asc_R_1: ",A(xr)," .NE. ",xr + num_images() * 1.5
            ERROR STOP 101
         end if
      end do
      print *,this_image(),":",A
   end subroutine asc_R_1

   subroutine asc_R_2(A, SIZE)
      real, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do xr = 1, SIZE
         do yr = 1, SIZE
            if (A(xr, yr) .NE. xr + yr + num_images() * 2.5) then
               print *,this_image(),": SUB:asc_R_2: ",A(xr, yr)," .NE. ",xr + yr + num_images() * 2.5
               ERROR STOP 102
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_R_2

   subroutine asc_R_3(A, SIZE)
      real, dimension(:,:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      do xr = 1, SIZE
         do yr = 1, SIZE
            do zr = 1, SIZE
               if (A(xr, yr, zr) .NE. xr + yr + zr + num_images() * 3.5) then
                  print *,this_image(),": SUB:asc_R_3: ",A(xr, yr, zr)," .NE. ",xr + yr + zr + num_images() * 3.5
                  ERROR STOP 103
               end if
            end do
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_R_3

end module asc_R

module asc_COMPLEX

   implicit none
   integer xcomplex, ycomplex, zcomplex

   contains

   subroutine asc_COMPLEX_1(A)
      complex, dimension(:), intent(in) :: A[*]
      complex :: temp
      do xcomplex = 1, SIZE(A)
         temp = (xcomplex + num_images() * 1.5, xcomplex + num_images() * 0.5)
         if (A(xcomplex) .NE. temp) then
            print *,this_image(),": SUB:asc_COMPLEX_1: ",A(xcomplex)," .NE. ",temp
            ERROR STOP 101
         end if
      end do
      print *,this_image(),":",A
   end subroutine asc_COMPLEX_1

   subroutine asc_COMPLEX_2(A, SIZE)
      complex, dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      complex :: temp
      do xcomplex = 1, SIZE
         do ycomplex = 1, SIZE
            temp = (xcomplex + ycomplex + num_images() * 2.5, xcomplex + ycomplex + num_images() * 1.5)
            if (A(xcomplex, ycomplex) .NE. temp) then
               print *,this_image(),": SUB:asc_COMPLEX_2: ",A(xcomplex, ycomplex)," .NE. ",temp
               ERROR STOP 102
            end if
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_COMPLEX_2

   subroutine asc_COMPLEX_3(A, SIZE)
      complex, dimension(:,:,:), intent(in) :: A[*]
      integer, intent(in) :: SIZE
      complex :: temp
      do xcomplex = 1, SIZE
         do ycomplex = 1, SIZE
            do zcomplex = 1, SIZE
               temp = (xcomplex + ycomplex + zcomplex + num_images() * 3.5, xcomplex + ycomplex + zcomplex + num_images() * 2.5)
               if (A(xcomplex, ycomplex, zcomplex) .NE. temp) then
                  print *,this_image(),": SUB:asc_COMPLEX_3: ",A(xcomplex, ycomplex, zcomplex)," .NE. ",temp
                  ERROR STOP 103
               end if
            end do
         end do
      end do
      print *,this_image(),":",A
   end subroutine asc_COMPLEX_3

end module asc_COMPLEX
