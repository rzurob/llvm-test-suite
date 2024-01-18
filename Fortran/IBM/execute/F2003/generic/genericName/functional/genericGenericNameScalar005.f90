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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic-name acts as structure constructor
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
      integer, pointer :: i
      integer, allocatable :: j
      contains
         procedure, nopass :: constrNoArg
         procedure, nopass :: constr1Arg
         procedure, nopass :: constr2Arg
        
   end type
   
   interface base
      module procedure constrNoArg
      module procedure constr1Arg
      module procedure constr2Arg
   end interface

   contains

      type(base) function constrNoArg ()

         allocate ( constrNoArg%i, source = -999 )
         allocate ( constrNoArg%j, source = -999 )

         print *, 'constrNoArg'

      end function
      
      type(base) function constr1Arg (a)
         integer, intent(in) :: a

         allocate ( constr1Arg%i, source = a )
         allocate ( constr1Arg%j, source = a )

         print *, 'constr1Arg'

      end function
      
      type(base) function constr2Arg (a,b)
         integer, intent(in) :: a, b

         allocate ( constr2Arg%i, source = a )
         allocate ( constr2Arg%j, source = b )

         print *, 'constr2Arg'

      end function

end module

program genericGenericNameScalar005
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3

   allocate ( b2, b3 )
   
   b1 = base()
   b2 = base(100)
   b3 = base(200, 300)
   
   print *, b1%i, b1%j
   print *, b2%i, b2%j
   print *, b3%i, b3%j
   
   call printtype( b1 )
   call printtype( base() )
   call printtype( base(1000) )
   call printtype( base(2000,3000) )

end program

subroutine printtype ( a )
   use m, only: base
   type(base), intent(in) :: a
   
   print *, a%i, a%j
   
end subroutine
