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
! %POSTCMD: dcomp comp002.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					  Components
!*                                        Components can be abstract type array(may or may not be zero size)
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
   end type

   type :: child1(k2)
      integer, kind :: k2
      type(base(k2)), dimension(5):: bb
   end type

   type :: child2(k3)
      integer, kind :: k3
      type(base(k3)), dimension(0):: bb
   end type

end module

program comp002

end program
