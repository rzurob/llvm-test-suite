! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/operator/functional/genericOperatorInheritance002.f
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
!*  DESCRIPTION                : Operator: testing the statement: (pg. 62 ln 1-2)
!*                                         If a generic binding specified in a type definition has the same generic-spec as an inherited binding, it
!*                                         extends the generic interface and shall satisfy the requirements specified in 16.2.3.
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
      integer(k1)      i
      contains
         generic :: operator(+) => addwbase
         procedure :: addwbase
   end type

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
      contains
         generic :: operator(+) => addwchild
         procedure :: addwchild
   end type

   contains

   class(base(:,4)) function addwbase (a, b)
      class(base(*,4)), intent(in) :: a
      type(base(*,4)), intent(in) :: b
      allocatable addwbase

      allocate ( addwbase, source = a )

      addwbase%i = a%i + b%i

      print *, 'addwbase'


   end function

   class(child(:,4,:,4)) function addwchild (a, b)

      class(child(*,4,*,4)), intent(in) :: a
      type(child(*,4,*,4)), intent(in)  :: b
      allocatable addwchild

      allocate ( addwchild, source = a )

      addwchild%i = addwchild%i + b%i
      addwchild%j = addwchild%j + b%j

      print *, 'addwchild'

   end function

end module

program genericOperatorInheritance002
   use m

   class(base(:,4)), allocatable :: b1, b2

   type (child(20,4,20,4)) :: c1
   class(child(:,4,:,4)), allocatable :: c2

   allocate ( b1, source = base(20,4)(10) )
   allocate ( b2, source = b1 + base(20,4)(10) )

   print *, b1%i, b2%i
   deallocate ( b1, b2 )

   c1 = child(20,4,20,4) ( 10, 11 )
   allocate ( c2, source = child(20,4,20,4) ( 20, 21 ) )

   allocate ( b1, source = child(20,4,20,4) ( 1, 10 ) + child(20,4,20,4) ( 2, 20 ) )  !<- extended type using parent type generic type bound

   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select

   allocate ( b2, source = c1 + c2 )

   select type ( b2 )
      type is ( child(*,4,*,4) )
         print *, b2%i, b2%j
   end select

   deallocate ( c2 )
   
   allocate ( c2, source = c1 + c1 )
   print *, c2%i, c2%j
   
   deallocate ( b2 )
   allocate ( b2, source = c1 + b1 )
   
   print *, b2%i


end program
