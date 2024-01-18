! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorElemental002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: with elemental function with class hierarchy
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
      integer(k1)   :: x = -999
      contains
         generic :: operator(*) => mul
         procedure :: mul
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: y = -999
      contains
         procedure :: mulchild
         generic :: operator(*) => mulchild
   end type

   contains

      elemental type(base(20,4)) function mul ( a, b )
         class(base(*,4)), intent(in) :: a
         type(base(*,4)), intent(in) :: b

         mul%x = a%x * b%x

      end function

      elemental type(child(20,4)) function mulchild ( a, b )
         class(child(*,4)), intent(in) :: a
         type(child(*,4)), intent(in) :: b

         mulchild%base = a%base * b%base
         mulchild%y = a%y * b%y

      end function

end module

program genericOperatorElemental002
   use m

   type(base(20,4)) :: b1, b2(3)
   class(base(:,4)), allocatable :: b3(:)

   type(child(20,4)) :: c1(3)
   class(child(:,4)), pointer :: c2(:)

   b1 = base(20,4)(2)
   b2 = (/ base(20,4)(3),  base(20,4)(4),  base(20,4)(5) /)

   allocate ( b3(3), source = ( b1 * b2 ) )
   print *, b3%x

   c1 = (/ child(20,4)(1,2), child(20,4)(3,4), child(20,4)(5,6) /)
   deallocate ( b3 )
   allocate ( b3(3), source =  b1 * c1%base )

   print *, b3%x

   deallocate ( b3 )
   allocate ( c2(3),source = (/ (child(20,4) (i, i+1) , i = 2,6,2 ) /) )
   allocate ( b3(3), source = (/ c1 * c2 /) )

   select type ( b3 )
      type is ( child(*,4) )
         print *, b3%x
         print *, b3%y
   end select

   deallocate ( b3, c2 )
   allocate ( c2(3), source = (/ ( child(20,4)(j,j*2), j = 3,1,-1 ) /) * (/ ( child(20,4)(j,j*2), j = 1,3 ) /) )

   print *, c2%x
   print *, c2%y

   allocate ( b3(2), source = child(20,4)(1,2) * (/ child(20,4)(2,3), child(20,4)(3,4) /) )

   select type ( b3 )
      type is ( child(*,4) )
         print *, b3%x
         print *, b3%y
   end select

end program
