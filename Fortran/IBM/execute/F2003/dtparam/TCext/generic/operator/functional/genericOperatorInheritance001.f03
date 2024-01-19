! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorInheritance001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
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
         generic :: operator(+) => add
         procedure :: add
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         generic :: operator(+) => addint
         procedure :: addint
   end type

   contains

   class(base(:,4)) function add (a, b)
      class(base(*,4)), intent(in) :: a, b
      allocatable add

      allocate ( add, source = a )

      select type ( b )
         type is ( base(*,4) )
            add%i = add%i + b%i
         type is ( child(*,4) )
            select type ( add )
               type is ( base(*,4) )
                  add%i = add%i + b%i
               type is ( child(*,4) )
                  add%i = add%i + b%i
                  add%j = add%j + b%j
            end select
      end select

   end function

   class(child(:,4)) function addint (a, b)
      class(child(*,4)), intent(in) :: a
      integer, intent(in)     :: b
      allocatable addint

      allocate ( addint, source = a )

      addint%i = addint%i + b
      addint%j = addint%j + b

   end function

end module

program genericOperatorInheritance001
   use m

   class(base(:,4)), allocatable :: b1, b2

   type (child(20,4)) :: c1
   class(child(:,4)), allocatable :: c2

   allocate ( b1, source = base(20,4)(10) )
   allocate ( b2, source = b1 + base(20,4)(10))

   print *, b1%i, b2%i
   deallocate ( b1, b2 )

   c1 = child(20,4) ( 10, 11 )
   allocate ( c2, source = child(20,4) ( 20, 21 ) )

   allocate ( b1, source = child(20,4) ( 1, 10 ) + child(20,4) ( 2, 20 ) )  !<- extended type using parent type generic type bound
   allocate ( b2, source = c1 + c2 )

   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
   end select

   deallocate ( c2 )
   allocate ( c2, source = c1 + 10 )
   print *, c2%i, c2%j

end program
