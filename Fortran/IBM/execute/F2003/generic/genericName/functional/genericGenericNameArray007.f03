!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb has allocatable/pointer dummy args
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

   type base
      integer :: i = -999
      contains
         procedure, nopass :: printallocbase1d
         procedure, nopass :: printptrbase2d

         generic :: print => printallocbase1d, printptrbase2d
   end type

   type, extends (base) :: child
      integer :: j = -9999
      contains
         procedure, nopass :: printallocchild1d
         procedure, nopass :: printptrchild2d

         generic :: print => printallocchild1d, printptrchild2d
   end type

   contains

      subroutine printallocbase1d (a)
         type(base), intent(inout), allocatable :: a(:)

         if ( .not. allocated(a) ) allocate ( a(4) )

         print *, 'printallocbase1d', a%i

      end subroutine

      subroutine printptrbase2d (a)
         type(base), intent(inout), pointer :: a(:,:)

         if ( .not. associated(a) ) allocate ( a(2,2) )

         print *, 'printptrbase2d', a%i

      end subroutine

      subroutine printallocchild1d (a)
         type(child), intent(inout), allocatable :: a(:)

         if ( .not. allocated(a) ) allocate ( a(4) )
         print *, 'printallocchild1d', a%i, a%j

      end subroutine

      subroutine printptrchild2d (a)
         type(child), intent(inout), pointer :: a(:,:)

         if ( .not. associated(a) ) allocate ( a(2,2) )
         print *, 'printptrchild2d', a%i, a%j

      end subroutine

end module

program genericGenericNameArray007
   use m

   type(base) :: b0, b1(:), b2(:,:)
   type(child) :: c0, c1(:), c2(:,:)

   allocatable :: b0, b1, c0, c1
   pointer :: b2, c2

   call b0%print(b1)
   call b0%print(b2)

   call c0%print(b1)
   call c0%print(b2)

   call c0%print(c1)
   call c0%print(c2)

   deallocate ( b1, b2, c1, c2 )
   allocate ( b1(5), source = (/ (base(i*100), i = 1, 5) /) )
   allocate ( b2(3,3), source = reshape ( source = (/ (base(i*200), i = 1, 9) /), shape = (/3,3/) ) )
   allocate ( c1(7), source = (/ (child(i*1000,i*2000), i = 1, 7) /) )
   allocate ( c2(3,3), source = reshape ( source = (/ (child(i*2000,i*4000), i = 1, 9) /), shape = (/3,3/) ) )

   call b0%print(b1)
   call b0%print(b2)

   call c0%print(b1)
   call c0%print(b2)

   call c0%print(c1)
   call c0%print(c2)

end program