! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/generic/operator/functional/genericOperatorRecursive001.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : operator: function being recursive and assigning linked lists
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
      integer, kind             :: k1
      integer, len              :: n1
      integer(k1)               :: i
      type(base(:,k1)), pointer :: next => null()
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   contains

   recursive function badd ( a, b )
      class(base(*,4)), intent(in) :: a
      type(base(*,4)), intent(in)   :: b
      type(base(20,4))::badd

      badd%i = a%i + b%i

      if ( associated ( b%next ) .and. associated ( a%next ) ) then
         allocate ( badd%next, source = a%next + b%next )
      end if

   end function

end module


program genericOperatorRecursive001
   use m

   type(base(20,4)), target :: b1
   type(base(20,4)), target :: b2, b3

   type(base(:,4)), pointer :: tmp => null()

   b1 = base(20,4)( 100, null() )
   tmp => b1

   do i = 1, 9
      allocate ( tmp%next, source = base(20,4)((i+1)*100, null() ) )
      tmp => tmp%next
   end do

   b2 = base(20,4)( 1000, null() )
   tmp => b2

   do i = 1, 9
      allocate ( tmp%next, source = base(20,4)((i+1)*1000, null() ) )
      tmp => tmp%next
   end do

   print *, 'Linked List 1'
   b3 = b1 + b2
   tmp => b3
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   print *, 'Linked List 2'
   b1 = b1 + b2 + b3
   tmp => b1
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   print *, 'Linked List 3'
   b2 = base(20,4)(10, null() ) + base(20,4)(20, null() )
   tmp => b2
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do


end program
