! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/operator/functional/genericOperatorScalar016d.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: type bound procedure in base and generic defined in
!*                                         child type, 
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
         procedure :: add
         procedure :: addwitharray
         generic :: operator(+) => add
   end type

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
      contains
         generic :: operator(+) => addwitharray
   end type

   contains

   class(base(:,4)) function add ( a, b )
      allocatable :: add
      class(base(*,4)), intent(in) :: a, b

      allocate ( add, source= base(20,4) ( i = a%i + b%i ) )
   end function

   class(base(:,4)) function addwitharray ( a, b )
      allocatable :: addwitharray(:)
      class(base(*,4)), intent(in) :: a, b(:)

      allocate ( base(20,4):: addwitharray(size(b)) )
      do i=1, size(b)
         addwitharray(i)%i =  a%i + b(i)%i
      end do
   end function

end module

program genericOperatorScalar016d
   use m
   
   type(base(20,4)) :: b1, b2(3)
   
   b1 = base(20,4)(1)
   b2 = base(20,4)(2)
   
   b2 = b1 + b2

end program
