! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDummyArg003.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : operator: poly dummy arguments being the operand
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
      integer(k1)   :: i =-999
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j =-999
      contains
         procedure, pass :: badd => cadd
   end type

   contains

   class(base(:,4)) function badd ( a, b )
      class(base(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      allocatable :: badd
      allocate ( base(20,4):: badd )

      badd%i = a%i + b%i

      print *, 'badd'

   end function

   class(base(:,4)) function cadd ( a, b )
      class(child(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      allocatable :: cadd
      allocate ( child(20,4) :: cadd )

      cadd%i = a%i + b%i
      select type ( b )
         type is ( child(*,4) )
            select type ( cadd )
               type is ( child(*,4) )
                  cadd%j = a%j + b%j
            end select
      end select

      print *, 'cadd'

   end function

end module

program genericOperatorDummyArg003
   use m

   class(base(:,4)), allocatable :: b1, b2
   class(child(:,4)), pointer :: c1, c2

   allocate ( b1, source = add ( base(20,4)(200), child(20,4)(300,400)))
   select type ( b1 )
      type is ( base(*,4) )
         print *, b1%i
   end select

   allocate ( b2, source = add ( child(20,4)(100,200), base(20,4)(300) ) )
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
   end select

   deallocate ( b1 )
   allocate ( b1, source = add ( child(20,4)(200, 300), child(20,4)(300,400)))
   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   allocate ( c1, source = child(20,4)(50,100))
   allocate ( c2, source = child(20,4)(500,1000) )

   deallocate ( b2 )
   allocate ( b2, source = add ( c1, c2 ) )
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
   end select

   contains

      class(base(:,4)) function add(a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b
         allocatable :: add
         print *, 'add'
         allocate (add,source = a+ b)

      end function

end program


