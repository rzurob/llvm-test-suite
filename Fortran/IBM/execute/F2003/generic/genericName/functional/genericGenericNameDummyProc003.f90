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
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             specified with an generic interface
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
      integer(4) :: i
      contains
         procedure, pass :: setint2
         procedure, pass :: setint4
         procedure, pass :: setint8
         procedure, pass :: setreal4
         procedure, pass :: setreal8
         generic :: set => setint2, setint4, setint8, setreal4, setreal8
   end type

   interface tointfour
      module procedure twotofour
      module procedure fourtofour
      module procedure eighttofour
      module procedure realeighttoint
      module procedure realfourtoint
   end interface

   contains

   subroutine setint2 ( dtv, proc, i )
      class(base), intent(inout) :: dtv
      procedure(twotofour) :: proc
      integer(2), intent(in) :: i

      dtv%i = proc(i)
      print *, 'setint2'

   end subroutine

   subroutine setint4 ( dtv, proc, i )
      class(base), intent(inout) :: dtv
      procedure(fourtofour) :: proc
      integer(4), intent(in) :: i

      dtv%i = proc(i)
      print *, 'setint4'

   end subroutine

   subroutine setint8 ( dtv, proc, i )
      class(base), intent(inout) :: dtv
      procedure(eighttofour) :: proc
      integer(8), intent(in) :: i

      dtv%i = proc(i)
      print *, 'setint8'

   end subroutine

   subroutine setreal4 ( dtv, proc, i )
      class(base), intent(inout) :: dtv
      procedure(realfourtoint) :: proc
      real(4), intent(in) :: i

      dtv%i = proc(i)
      print *, 'setreal4'

   end subroutine

   subroutine setreal8 ( dtv, proc, i )
      class(base), intent(inout) :: dtv
      procedure(realeighttoint) :: proc
      real(8), intent(in) :: i

      dtv%i = proc(i)
      print *, 'setreal8'

   end subroutine

   integer(4) function twotofour(i)
      integer(2), intent(in) :: i

      twotofour = int(i,kind=4)
      print *, 'twotofour'

   end function

   integer(4) function fourtofour(i)
      integer(4), intent(in) :: i

      fourtofour = int(i,kind=4)
      print *, 'fourtofour'

   end function

   integer(4) function eighttofour(i)
      integer(8), intent(in) :: i

      eighttofour = int(i,kind=4)
      print *, 'eighttofour'

   end function

   integer(4) function realfourtoint(r)
      real(4), intent(in) :: r

      realfourtoint = int(r,kind=4)
      print *, 'realfourtoint'

   end function

   integer(4) function realeighttoint(r)
      real(8), intent(in) :: r

      realeighttoint = int(r,kind=4)
      print *, 'realeighttoint'

   end function

end module

program genericGenericNameDummyProc003
   use m

   type(base), pointer :: b1
   type(base), target :: b2

   integer(8), parameter :: TENTHOUSAND = 10000

   b1 => b2

   call b1%set(twotofour, 10_2)
   print *, b1%i, b2%i

   call b1%set(fourtofour, 100_4)
   print *, b1%i, b2%i

   call b1%set(eighttofour, TENTHOUSAND )
   print *, b1%i, b2%i

   call b2%set(realfourtoint, 100000.0_4)
   print *, b1%i, b2%i

   call b2%set(realeighttoint, 9999999.0_8)
   print *, b1%i, b2%i

end program
