! GB DTP extension using:
! ftcx_dtp -qdeferredlp -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent001.f
! opt variations: -qck -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: intrinsic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
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

   type base(k1,n1)    ! (4,5)
      integer, kind              :: k1
      integer, len               :: n1
      integer(k1), allocatable   :: i
      character(n1), allocatable :: c
      real(k1), allocatable      :: r

      contains

         procedure, pass :: geti
         procedure, pass :: getc
         procedure, pass :: getr

   end type

   type(base(4,:)), pointer :: b3

   contains

      integer function geti ( a )
         class(base(4,*)), intent(in) :: a
         geti = a%i
      end function

      character(5) function getc ( a )
         class(base(4,*)), intent(in) :: a
         getc = a%c
      end function


      real function getr ( a )
         class(base(4,*)), intent(in) :: a
         getr = a%r
      end function

      subroutine foo( dtv )
         type(base(4,5)), value :: dtv

         print *, dtv%geti(), dtv%getc(), dtv%getr()
         dtv = base(4,5)( -999, 'xxxxx', -999.0 )
         print *, dtv%geti(), dtv%getc(), dtv%getr()

      end subroutine

end module

program valueScalarAllocatableComponent001
   use m

   type(base(4,5)) :: b1
   type(base(4,:)), allocatable :: b2

   b1 = base(4,5)(1,'house', 2.0)
   allocate ( b2, source = base(4,5)(3,'mouse',4.0) )
   allocate ( b3, source = base(4,5)(5,'apple',6.0) )

   call foo( b1 )
   print *, b1%i, b1%c, b1%r
   call foo( b2 )
   print *, b2%i, b2%c, b2%r
   call foo( b3 )
   print *, b3%i, b3%c, b3%r

end program
