! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/genericName/functional/genericGenericNameArray002.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb containing array of
!*                                             derived types dummy args of different ranks
!*                                             and use pass in first and other dummy args
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   type base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i(:)
      contains
         procedure, pass(a) :: b_barray
         procedure, pass(b) :: b_b

         generic :: c => b_barray, b_b
   end type

   contains

   subroutine b_barray( a, b )
      class(base(4)), intent(inout) :: a
      class(base(4)), intent(in) :: b(:)

      integer :: sizeof, aindex

      if ( allocated ( a%i ) ) deallocate ( a%i )

      sizeof = 0
      do i = 1,size(b)
         sizeof = size(b(i)%i) + sizeof
      end do

      allocate ( a%i(sizeof) )
      aindex = 1
      do j = 1, size(b)
         do k = 1, size(b(j)%i)
            a%i(aindex) = b(j)%i(k)
            aindex = aindex + 1
         end do
      end do

      print *, 'b_barray'

   end subroutine

   subroutine b_b( a, b )
      class(base(4)), intent(inout) :: b
      class(base(4)), intent(in) :: a

      if ( allocated ( b%i ) ) deallocate ( b%i )

      allocate ( b%i(size(a%i)), source = a%i )
      print *, 'b_b'
   end subroutine


end module

program genericGenericNameArray002
   use m

   class(base(4)), allocatable :: b1, b2(:)

   type(base(4)) :: b3, b4

   allocate ( b1, b1%i(5) )
   allocate ( b2(10), source = (/( base(4) ( (/i, i+1, i+2, i+3, i+4, i+5 /) ), i = 1, 10 )/) )

   do i = 1, size(b2)
      print *, b2(i)%i
   end do
   call b1%c(b2)

   print *, b1%i

   call b3%c(b1)
   print *, b3%i
   call b4%c(b3)
   print *, b4%i

   deallocate ( b2 )

   allocate ( b2(5) )

   allocate ( b2(1)%i(5) , source = (/1,2,3,4,5/) )
   allocate ( b2(2)%i(4) , source = (/6,7,8,9/) )
   allocate ( b2(3)%i(3) , source = (/10,11,12/) )
   allocate ( b2(4)%i(2) , source = (/13,14/) )
   allocate ( b2(5)%i(1) , source = (/15/) )

   do i = 1, size(b2)
      print *, b2(i)%i
   end do

   call b1%c(b2)
   print *, b1%i

   call b3%c(b2(1:5:2)) !array section
   print *, b3%i

   call b4%c(b2( (/5,4,3,2,1,2,3,4,5/)) ) !array section
   print *, b4%i

   call b1%c(b2(5)) !array element
   print *, b1%i

end program