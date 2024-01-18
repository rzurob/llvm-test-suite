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
!*  DESCRIPTION                : Binary Operator: UD operator subroutine is a pure function, with class hierarchy and
!*                                                child type defined another UD operator
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
      integer :: x = -999
      contains
         procedure, pass :: mybadd
         generic :: operator(*) => mybadd
   end type

   type, extends(base) :: child
      integer :: y = -999
      contains
         procedure, pass :: mybaddarray
         generic :: operator(*) => mybaddarray
   end type

   contains

   class(base) pure function mybadd ( a, b )
      class(base), intent(in) :: a
      integer, intent(in) :: b
      allocatable :: mybadd

      select type ( a )
         type is ( base )
            allocate ( mybadd )
            mybadd%x = a%x * b
         type is ( child )
            allocate ( child :: mybadd )
            select type ( mybadd )
               type is ( child )
                  mybadd%x = a%x * b
                  mybadd%y = a%y * b
            end select
      end select

   end function

   class(base) pure function mybaddarray ( a, b )
      class(child), intent(in) :: a
      integer, intent(in) :: b(:)
      allocatable :: mybaddarray

      allocate ( child :: mybaddarray )
      select type ( mybaddarray )
         type is ( child )
            mybaddarray%x = a%x * b(1)
            mybaddarray%y = a%y * b(1)
      end select

   end function

end module

program genericOperatorPure002
   use m

   type(base) :: b1
   class(base), allocatable :: b2
   class(base), pointer :: b3

   b1 = base(100) * 10
   allocate ( b2, source = b1 * 100  )
   allocate ( b3, source = child(100,200) * (/ 100, 200 /) )

   print *, b1%x  
   print *, b2%x
   select type ( b3 )
      type is ( child )
         print *, b3%x, b3%y
   end select

end program
