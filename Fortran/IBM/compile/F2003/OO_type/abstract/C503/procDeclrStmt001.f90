!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: dcomp procDeclrStmt001.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure declaration statement
!*                                        external functions (with implicit interface) 
!*                                        with return type which is non-polymorphic abstract type
!*                                           
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
   integer i
end type

type, extends(base) :: child
end type

end module

program procDeclrStmt001
   use m
   
   class(base), pointer :: b1
   procedure(type(base)) :: genbaseptr
   allocate(b1, source=genbaseptr())
   print *,b1%i
   call printbase( genbaseptr() )

contains

   subroutine printbase(a)
      use m
      class(base), intent(in) :: a
      print *,a%i
   end subroutine
   
end program

type(base) function genbaseptr()
   use m
   genbaseptr=base(5)
end function

