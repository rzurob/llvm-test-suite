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
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DRIVER STANZA              : xlf2003
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

   type base
      integer(4) :: i =-999
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   type, extends(base) :: child
      integer :: j =-999
      contains
         procedure, pass :: badd => cadd
   end type

   contains

   class(base) function badd ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b

      allocatable :: badd
      allocate ( badd )

      badd%i = a%i + b%i

      print *, 'badd'

   end function
   
   class(base) function cadd ( a, b )
      class(child), intent(in) :: a
      class(base), intent(in) :: b

      allocatable :: cadd
      allocate ( child :: cadd )

      cadd%i = a%i + b%i
      select type ( b )
         type is ( child )
            select type ( cadd )
               type is ( child )
                  cadd%j = a%j + b%j
            end select
      end select

      print *, 'cadd'

   end function

end module

program genericOperatorDummyArg003
   use m

   class(base), allocatable :: b1, b2
   class(child), pointer :: c1, c2
   
   allocate ( b1, source = add ( base(200), child(300,400)))
   select type ( b1 )
      type is ( base )
         print *, b1%i
   end select
   
   allocate ( b2, source = add ( child(100,200), base(300) ) )
   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%j
   end select
   
   deallocate ( b1 )
   allocate ( b1, source = add ( child(200, 300), child(300,400)))
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select
   
   allocate ( c1, source = child(50,100))
   allocate ( c2, source = child(500,1000) )
   
   deallocate ( b2 )
   allocate ( b2, source = add ( c1, c2 ) )
   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%j
   end select
   
   contains

      class(base) function add(a, b)
         class(base), intent(in) :: a
         class(base), intent(in)  :: b
         allocatable :: add
         print *, 'add'
         allocate (add,source = a+ b)

      end function

end program


