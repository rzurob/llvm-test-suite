!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: implicit005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Abstract type with IMPLICIT STATEMENT
!*                                        IMPLICIT legal polymorphic abstract type
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(getidif), pass, deferred :: getid
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
   contains
      procedure, pass :: getid
   end type

   interface
      integer function getidif(a)
         import base
         class(base(4)), intent(in) :: a
      end function
   end interface

contains

   integer function getid(a)
      class(child(4,4)), intent(in) :: a
      getid = a%id
   end function

end module

program implicit005

   use m
   IMPLICIT class(base(4)) (A-B)
   pointer :: Aa,Ab
   allocatable :: Ba,Bb

   type(child(4,4)) :: c1 = child(4,4)(5)
   class(child(4,4)),allocatable :: c2
   allocate(c2, source = child(4,4)(6) )

   allocate(Aa, source = c1)
   allocate(Ab, source = child(4,4)(2) )
   allocate(Ba, source = c2)
   allocate(Bb, source = child(4,4)(3) )

   if ( Aa%getid() .ne. c1%id ) error stop 1_4
   if ( Ba%getid() .ne. c2%id ) error stop 2_4
   if ( Ab%getid() .ne. 2 )     error stop 3_4
   if ( Bb%getid() .ne. 3 )     error stop 4_4

end program
