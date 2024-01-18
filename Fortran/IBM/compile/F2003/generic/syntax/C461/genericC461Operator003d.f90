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
!*  DESCRIPTION                : C461: Generic type bound with operator and do not
!*                                     specify pass object dummy argument (user defined operator)
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

   type, abstract :: base
      integer :: i = -1
   contains
      procedure(lessthaninf), deferred, nopass :: lessthan
      generic :: operator(.lt.) => lessthan
      procedure, nopass :: myproc
      generic :: operator(.myproc.) => myproc
   end type

   interface
      logical function lessthaninf(a,b)
         import base
         class(base), intent(in) :: a, b
      end function
   end interface
   
   contains
   
      class(base) function myproc (a)
         class(base), intent(in) :: a
         allocatable :: myproc
         allocate ( myproc, source = a )
      end function

end module

module child
   use m

   type, extends(base) :: child
      integer :: j = -1
   contains
      procedure, nopass :: lessthan => childlessthan
   end type

   contains

      logical function childlessthan (a, b)
         class(child), intent(in) :: a, b
         childlessthan = ( ( a%i .lt. b%i ) .and. ( a%j .lt. b%j ) )
      end function

end module


program genericC461Operator003d
end program
