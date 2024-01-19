! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/generic/mix/genericMix005.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DESCRIPTION                : mix generic interface with UD and intrinsic procedures
!*
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

   type :: ptr(n1,k1)    ! (20,4)
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: jj
      contains
         procedure, pass :: ptrnull
         generic :: null => ptrnull
   end type

   type base(n2,k2,k3)    ! (20,4,8)
      integer, kind            :: k2,k3
      integer, len             :: n2
      integer(k2), pointer     :: i   => null()
      type(ptr(:,k2)), pointer :: p  => null()
      real(k3), pointer        :: r => null()
   end type

   interface null
      procedure mynull
   end interface

   contains

      type(base(:,4,8)) function mynull ( r )
         type(base(:,4,8)), intent(inout), pointer :: r
         pointer :: mynull

         r%i => null()
         call r%p%null()
         r%p => null()
         r%r => null()

         r => null()

         mynull => r

         print *, 'mynull'

      end function

      subroutine ptrnull(a)
         class(ptr(*,4)), intent(inout) :: a

         nullify ( a%jj )

      end subroutine

end module

program genericMix005
   use m

   type(base(:,4,8)), pointer :: b1, b2

   integer, pointer :: i

   allocate ( i, source = 20 )

   allocate ( b1, source=base(20,4,8)() )
   allocate ( b1%i , source = 10_4 )
   allocate ( b1%p , source = ptr(20,4)(b1%i) )
   allocate ( b1%r , source = 20.0_8 )

   print *, associated ( b1 ), associated ( b1%i ), associated ( b1%p ), associated ( b1%p%jj ), associated ( b1%r )

   b2 => null(b1)

   print *, associated ( b1 )!, associated ( b1%i ), associated ( b1%p ), associated ( b1%p%jj ), associated ( b1%r )
   print *, associated ( b2 )!, associated ( b2%i ), associated ( b2%p ), associated ( b2%p%jj ), associated ( b2%r )

   print *, associated ( i )
   i => null(i)
   print *, associated ( i )

end program
