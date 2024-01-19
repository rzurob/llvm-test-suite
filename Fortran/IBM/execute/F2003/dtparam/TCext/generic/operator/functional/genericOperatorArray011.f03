! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorArray011.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: with array and class hierarchy
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
      integer(k1)   :: i
      contains
         procedure :: addwitharray
         procedure :: addwithintarray   !<- generic only be used in type child
         generic :: operator(+) => addwitharray
   end type

   type, extends(base) :: child    ! (20,4)
      contains
         generic :: operator(+) => addwithintarray
   end type

   contains

   class(base(:,4)) function addwithintarray ( a, b )
      allocatable :: addwithintarray(:)
      class(base(*,4)), intent(in) :: a
      integer, intent(in) :: b(:)

      allocate ( base(20,4):: addwithintarray(size(b)) )

      do i=1, size(b)
         addwithintarray(i)%i = a%i + b(i)
      end do

   end function

   class(base(:,4)) function addwitharray ( a, b )
      allocatable :: addwitharray(:)
      class(base(*,4)), intent(in) :: a, b(:)

      allocate ( child(20,4) :: addwitharray(size(b)) )

      do i=1, size(b)
         addwitharray(i)%i  =  a%i + b(i)%i
      end do

   end function

end module

program genericOperatorArray011
   use m

   class(base(:,4)), allocatable :: b1(:), b3(:)
   type(base(20,4)) :: b2(3)

   allocate ( b1(3), source = (/ (child(20,4)(i), i = 11, 13) /) )

   b2 =  b1(1)+ b1
   print *, b2%i

   select type ( g => b1(1) )
      type is (child(*,4))
         allocate ( b3(3), source = g + (/ -10, -11, -12 /) )
         print *, b3%i
   end select

   deallocate (b3)

   allocate ( b3(3), source = addwithintarray( b1(1), (/ -20, -21, -22 /) ) )
   print *, b3%i

end program
