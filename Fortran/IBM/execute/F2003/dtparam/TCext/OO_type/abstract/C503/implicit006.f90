!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: implicit006.f
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
!*                                        IMPLICIT legal polymorphic non-abstract type that is extension of abstract type
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
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
   end type

end module

program implicit006
   use m
   IMPLICIT class(child(4,4)) (A-F)

   pointer Aa
   allocatable Bb

   type(child(4,4)) :: c1 = child(4,4)(5)
   class(child(4,4)), allocatable :: c2

   allocate(c2, source = child(4,4)(5) )

   allocate(Aa, source = c1)
   allocate(Bb, source = c2)

   if ( Aa%id .ne. c1%id ) error stop 1_4
   if ( Bb%id .ne. c1%id ) error stop 2_4

end program
