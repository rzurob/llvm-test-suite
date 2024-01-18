c************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qstrict -qrndsngl -qnomaf
! %GROUP: fxepmp01.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
!************************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 1, 1998
!*  MODIFIED BY                :
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  REFERENCE                  :
!*
!*  DESCRIPTION                :  Use complex array pointers,
!*  passed as actual arguments from module procedures, to external
!*  user-defined elemental procedures.
!*  The main program, uses the module, and makes calls to the module
!*  procedures.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  09/01/98   CR     -Initial release
!*  12/01/10   GB     -Copy from $(tsrcdir)elemental/epmp/*.f - feature 384867
!*                     changing intent(in) to value for non arrays dummy args
!* ===================================================================
!*
!238567169012385671690123856716901238567169012385671690123856716901238567169012

      module mod1
      contains
c *****************************************
      subroutine sub1
c *****************************************
      interface
c *****************************************
      elemental function power(arr,n)
         complex, value :: arr
         integer, value :: n
         complex :: power
      end function power
c *****************************************
      elemental function logn(arr,n)
          complex*16, value :: arr
          integer, value :: n
          complex*16 :: logn
      end function logn
c *****************************************
      function Flogn(arr,n)
          complex*16, value :: arr
          integer, value :: n
          complex*16 :: Flogn
      end function Flogn
c *****************************************
      end interface
c *****************************************
      integer  i, j, k, l, m
c *****************************************
c Declare complex array pointers
c *****************************************
      complex, pointer :: cptr1a(:)
      complex*16, pointer :: cptr16a(:,:,:,:)
c *****************************************
c Declare complex arrays
c *****************************************
      complex, target, dimension(5) :: comp1a
      complex, dimension(5) :: cres1a, c1a
      complex*16, target, dimension(5,5,5,5) :: comp16a, cres16a
c *****************************************
c Assign the pointer to the appropriate arrays
c *****************************************
      cptr1a => comp1a
      cptr16a => comp16a
c *****************************************
c Now initialize values of arrays, including some zero values
c *****************************************
      do i = 1,5
         comp1a(i)=(i-1.0,i)
         do j = 1,5
            do k = 1,5
               comp16a(i,j,k,:)=(k-1.0,k)
            end do
         end do
      end do
c *****************************************
c Call elemental procedures
c *****************************************
      do i=1,5
         cres1a = power(cptr1a,i)
         c1a = cptr1a**i
         if (any(cres1a .ne. c1a)) then
           error stop 1
         end if
      end do
c *****************************************
      do i=2,9,3

       cres16a = logn(cptr16a,i)
       do j=1,5
         do k=1,5
           do l=1,5
             do m=1,5
       if (cres16a(j,k,l,m) .ne.
     +      Flogn(cptr16a(j,k,l,m),i)) then
         error stop 2
       end if
             end do
           end do
         end do
       end do

      end do
c *****************************************
      end subroutine sub1
c *****************************************
      subroutine sub2
c *****************************************
      interface
c *****************************************
      elemental function power8(arr,n)
         complex*8, value :: arr
         integer, value :: n
         complex*8 :: power8
      end function power8
c *****************************************
      elemental function inv(arr)
         complex*32, value :: arr
         complex*32 :: inv
      end function inv
c *****************************************
      function Finv(arr)
         complex*32, value :: arr
         complex*32 :: Finv
      end function Finv
c *****************************************
      end interface
c *****************************************
      integer  i, j, k, l
c *****************************************
c Declare complex array pointers
c *****************************************
      complex*8, pointer :: cptr8a(:,:)
      complex*32, pointer :: cptr32a(:,:,:,:,:,:,:)
c *****************************************
c Declare complex arrays
c *****************************************
      complex*8, target, dimension(5,5) :: comp8a, cres8a, c8a
      complex*32, target, dimension(5,5,5,5,5,5,5) :: comp32a, cres32a
c *****************************************
c Assign the pointer to the appropriate arrays
c *****************************************
      cptr8a => comp8a
      cptr32a => comp32a
c *****************************************
c Now initialize values of arrays, including some zero values
c *****************************************
      do i = 1,5
         do j = 1,5
            comp8a(i,j)=(j-1.0,j)
            do k = 1,5
               do l = 1,5
                  comp32a(i,j,k,l,:,:,:)=(l-1.0,l)
               end do
            end do
         end do
      end do
c *****************************************
c Call elemental procedures
c *****************************************
      do i=1,5
         cres8a = power8(cptr8a,i)
         c8a=cptr8a**i
         if (any(cres8a .ne. c8a)) then
           error stop 10
         end if
      end do
c *****************************************
      cres32a = inv(cptr32a)
       do j=1,5
         do k=1,5
           do l=1,5
             do m=1,5
               do n=1,5
                 do o=1,5
                   do p=1,5
       if (cres32a(j,k,l,m,n,o,p) .ne.
     +      Finv(cptr32a(j,k,l,m,n,o,p))) then
         error stop 11
       end if
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
c *****************************************
      end subroutine sub2
c *****************************************
      end module mod1
c *****************************************
      program fxepmp01
      use mod1

      call sub1
      call sub2

      end program fxepmp01
c *****************************************
c **** Elemental Functions ****************
c *****************************************
      elemental function power(arr,n)
         complex, value :: arr
         integer, value :: n
         complex :: power
         power = arr ** n
      end function power
c *****************************************
      elemental function power8(arr,n)
         complex*8, value :: arr
         integer, value :: n
         complex*8 :: power8
         power8 = arr ** n
      end function power8
c *****************************************
      elemental function inv(arr)
         complex*32, value :: arr
         complex*32 :: inv
         if (arr .ne. 0) then
            inv = 1 / arr
         else
            inv = cmplx(0.0)
         end if
      end function inv
c *****************************************
      elemental function logn(arr,n)
          complex*16, value :: arr
          integer, value :: n
          complex*16 :: logn
          if ((n .eq. 1) .or. (real(arr) .eq. 0.0)) then
             logn = cmplx(0.0)
          else
             logn = cmplx(log10(real(arr)) / log10(real(n)))
          end if
      end function logn
c *****************************************
c ***** Non-elemental Functions ***********
c *****************************************
      function Flogn(arr,n)
          complex*16, value :: arr
          integer, value :: n
          complex*16 :: Flogn
          if ((n .eq. 1) .or. (real(arr) .eq. 0.0)) then
             Flogn = cmplx(0.0)
          else
             Flogn = cmplx(log10(real(arr)) / log10(real(n)))
          end if
      end function Flogn
c *****************************************
      function Finv(arr)
         complex*32, value :: arr
         complex*32 :: Finv
         if (arr .ne. 0) then
            Finv = 1 / arr
         else
            Finv = cmplx(0.0)
         end if
      end function Finv
c *****************************************
