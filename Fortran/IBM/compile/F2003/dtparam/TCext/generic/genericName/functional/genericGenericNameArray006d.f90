! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/genericName/functional/genericGenericNameArray006d.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb has allocatable/pointer dummy args, see if
!*                                             actual arg is checked to match when generic tb is called
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

   type base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c
      contains
         procedure, nopass :: printallocbase1d
         procedure, nopass :: printptrbase2d

         generic :: print => printallocbase1d, printptrbase2d
   end type

   type, extends (base) :: child(n2,k2)    ! (4,3,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i =-999
   end type

   contains

      subroutine printallocbase1d (a)
         class(base(4,*)), intent(in), allocatable :: a(:)

      end subroutine

      subroutine printptrbase2d (a)
         class(base(4,*)), intent(in), pointer :: a(:,:)

      end subroutine

end module

program genericGenericNameArray006d
   use m

   class(base(4,:)) :: b0, b1(:), b2(:,:)
   pointer :: b0, b1
   allocatable :: b2

   allocate ( base(4,20):: b0, b1(10) )

   call b0%print(b1)
   call b0%print(b2)

end program
