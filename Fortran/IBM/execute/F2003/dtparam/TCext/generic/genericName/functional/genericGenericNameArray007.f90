! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameArray007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, nopass :: printallocbase1d
         procedure, nopass :: printptrbase2d

         generic :: print => printallocbase1d, printptrbase2d
   end type

   type, extends (base) :: child    ! (20,4)
      integer(k1) :: j = -9999
      contains
         procedure, nopass :: printallocchild1d
         procedure, nopass :: printptrchild2d

         generic :: print => printallocchild1d, printptrchild2d
   end type

   contains

      subroutine printallocbase1d (a)
         type(base(*,4)), intent(inout), allocatable :: a(:)

         if ( .not. allocated(a) ) allocate ( a(4) )

         print *, 'printallocbase1d', a%i

      end subroutine

      subroutine printptrbase2d (a)
         type(base(*,4)), intent(inout), pointer :: a(:,:)

         if ( .not. associated(a) ) allocate ( a(2,2) )

         print *, 'printptrbase2d', a%i

      end subroutine

      subroutine printallocchild1d (a)
         type(child(*,4)), intent(inout), allocatable :: a(:)

         if ( .not. allocated(a) ) allocate ( a(4) )
         print *, 'printallocchild1d', a%i, a%j

      end subroutine

      subroutine printptrchild2d (a)
         type(child(*,4)), intent(inout), pointer :: a(:,:)

         if ( .not. associated(a) ) allocate ( a(2,2) )
         print *, 'printptrchild2d', a%i, a%j

      end subroutine

end module

program genericGenericNameArray007
   use m

   type(base(20,4)) :: b0, b1(:), b2(:,:)
   type(child(20,4)) :: c0, c1(:), c2(:,:)

   allocatable :: b0, b1, c0, c1
   pointer :: b2, c2

   call b0%print(b1)
   call b0%print(b2)

   call c0%print(b1)
   call c0%print(b2)

   call c0%print(c1)
   call c0%print(c2)

   deallocate ( b1, b2, c1, c2 )
   allocate ( b1(5), source = (/ (base(20,4)(i*100), i = 1, 5) /) )
   allocate ( b2(3,3), source = reshape ( source = (/ (base(20,4)(i*200), i = 1, 9) /), shape = (/3,3/) ) )
   allocate ( c1(7), source = (/ (child(20,4)(i*1000,i*2000), i = 1, 7) /) )
   allocate ( c2(3,3), source = reshape ( source = (/ (child(20,4)(i*2000,i*4000), i = 1, 9) /), shape = (/3,3/) ) )

   call b0%print(b1)
   call b0%print(b2)

   call c0%print(b1)
   call c0%print(b2)

   call c0%print(c1)
   call c0%print(c2)
   
end program
